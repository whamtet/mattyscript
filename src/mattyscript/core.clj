(ns mattyscript.core
  (:require [hawk.core :as hawk]
            [mattyscript.util :as util]
            )
  (:refer-clojure :exclude [compile]))

(import java.io.File)

(def project-version (-> "project.clj" slurp read-string (nth 2)))

(defn spit-script [dir ns s]
  (let [
         elements (.split (str ns) "\\.")
         parent (apply str (interpose "/" (concat [dir] elements)))
         f (str parent "/index.js")
         ]
    (.mkdirs (File. parent))
    (spit f (format "//Compiled by Mattyscript %s\n%s" project-version s))))

(defn read-file [f]
  (read-string (format "[%s]" (slurp f))))

(defn subdirs [f]
  (filter #(.isDirectory %) (file-seq (File. f))))

(defn safe-watcher
  "watcher that avoids duplicate calls in close succession"
  [paths handler]
  (let [
         recent (atom #{})
         handler2 (fn [ctx e]
                    (when-not (@recent e)
                      (swap! recent conj e)
                      (future
                        (Thread/sleep 100)
                        (swap! recent disj e))
                      (try (handler e) (catch Exception e (println e)))))
         ]
    (hawk/watch! [{:paths (mapcat subdirs paths)
                   :filter (fn [_ {:keys [file]}] (and (.isFile file) (.endsWith (.getName file) ".clj")))
                   :handler handler2}])))

(declare handler)
(declare compile)
(defonce watcher (safe-watcher ["src-mattyscript"] #'handler))

(defn export-format [form s & args]
  (let [
         prefix (if (:export (meta form)) "export " "")
         ]
    (str prefix (apply format s args))))

#_(defn interpose-lines [lines]
    (apply str (interpose "\n" (filter identity lines))))

(defn map-str [f & args]
  (apply str (apply map f args)))

(def special-symbols {"+" "_PLUS_"
                      "?" "_QMARK_"
                      "-" "_"
                      "#" "_HASH_"})

(defn compile-symbol [symbol]
  (reduce (fn [s [a b]] (.replace s a b)) (str symbol) special-symbols))

(def special-forms '{+ " + " - " - " / " / " * " * " and " && " or " || "})
(defn compile-special-form [type args]
  ;(println "compile-special-form" type args)
  (apply str
         (interpose (special-forms type) (map #(str "(" (compile %) ")") args))))

;;
;; Destructure variable binding
;;

(declare compile-vector-arg compile-map-arg)
(defn compile-arg [parent-var i v]
  (cond
    (symbol? v) (format "var %s = %s[%s]\n" (compile-symbol v) parent-var i)
    (vector? v)
    (let [s (str (gensym))]
      (format "var %s = %s[%s]\n%s" s parent-var i (compile-vector-arg s v)))
    (map? v)
    (let [s (str (gensym))]
      (format "var %s = %s[%s]\n%s" s parent-var i (compile-map-arg s v)))
    :default
    (throw (Exception. (format "Impossible compile-arg %s %s %s" parent-var i v)))))

(defn compile-let-arg [[k v]]
  (cond
    (symbol? k) (format "var %s = %s\n" (compile-symbol k) (compile v))
    (vector? k)
    (let [s (str (gensym))]
      (format "var %s = %s\n%s" s (compile v) (compile-vector-arg s k)))
    (map? v)
    (let [s (str (gensym))]
      (format "var %s = %s\n%s" s (compile v) (compile-map-arg s k)))
    :default
    (throw (Exception. (format "Impossible let-arg %s %s" k v)))))

(defn compile-let-args [binding-vector]
  (map-str compile-let-arg (partition 2 binding-vector)))

(defn compile-map-arg [parent-var {:keys [as strs] :as m}]
  (str
    (if as
      (format "var %s = %s\n" (compile-symbol as) parent-var))
    (apply str
           (for [s strs :let [s2 (compile-symbol s)]]
             (format "var %s = %s['%s']\n" s2 parent-var s)))
    (apply str
           (for [[k v] m :when (string? v)]
             (compile-arg parent-var (pr-str v) k)))))

(defn after [x s]
  (nth (drop-while #(not= x %) s) 1))

(defn compile-vector-arg [parent-var v]
  (let [
         normal-args (take-while #(not (#{'& :as} %)) v)
         ]
    (str
      (map-str #(compile-arg parent-var %1 %2) (range) normal-args)
      (if (some #(= :as %) v)
        (format "var %s = %s\n"
                (compile-symbol (after :as v))
                parent-var))
      (if (some #(= '& %) v)
        (format "var %s = %s.slice(%s)\n"
                (after '& v)
                (compile-symbol parent-var)
                (count normal-args))))))

(defn compile-arg-list [v]
  (format "var args = Array.from(arguments)\n%s" (compile-vector-arg "args" v)))

;;
;; end Destructure
;;

(defn map-last [f1 f2 s]
  (if (not-empty s)
    (concat (map f1 (butlast s)) [(f2 (last s))])))

(defn do-statements [statements]
  (apply str
         (map-last
           #(str (compile %) "\n")
           #(str "return " (compile %) "\n")
           statements)))

(defn compile-fn [[_ name arg-list & forms]]
  (let [
         [name arg-list forms]
         (if (vector? name)
           ["function" name (conj forms arg-list)]
           [name arg-list forms])
         ]
    (if (every? symbol? arg-list)
      (format "%s(%s){\n%s}\n" name (apply str (interpose ", " arg-list)) (do-statements forms))
      (format "%s(){\n%s%s}\n" name (compile-arg-list arg-list) (do-statements forms)))))

(defn compile-do [statements]
  (format "(function(){%s}())" (do-statements statements)))

(defn compile-invoke [form]
  (let [
         [dot obj method & method-args] (macroexpand-1 form)
         [f & args] form
         ]
    (if (= '. dot)
      (format "%s.%s(%s)" (compile obj) method (apply str (interpose ", " (map compile method-args))))
      (format "%s(%s)\n" (compile f) (apply str (interpose ", " (map compile args)))))))

(defn compile-if [[cond then else]]
  (if else
    (format "(function() {if (%s) {return %s} else {return %s}}())" (compile cond) (compile then) (compile else))
    (format "(function() {if (%s) {return %s}}())" (compile cond) (compile then))))

(defn compile-let [[binding-vector & body]]
  (format "(function() {\n%s%s}())" (compile-let-args binding-vector) (do-statements body)))

(defn compile-seq [[type & args :as form]]
  ;(println "compile-seq" form)
  (cond
    ;;
    ;; def
    ;;
    (= 'def type)
    (let [[a b] args]
      (export-format form "var %s = %s\n" a (compile b)))
    ;;
    ;; set
    ;;
    (= 'set! type)
    (let [[a b] args]
      (format "%s = %s\n" a (compile b)))
    ;;
    ;; import
    ;;
    (= 'import type)
    (let [[path _ imports] args
          args (apply str (interpose ", " imports))]
      (export-format form "import { %s } from '%s'\n" args path))
    ;;
    ;; class
    ;;
    (= 'class type)
    (let [[name superclass & methods] args]
      (export-format form "class %s extends %s {\n\n%s}" name superclass (map-str compile-fn methods)))
    ;;
    ;; fn
    ;;
    ('#{fn fn*} type)
    (compile-fn form)
    ;;
    ;; defn
    ;;
    (= 'defn type)
    (let [[name & args] args]
      (export-format form "var %s = %s\n" name (compile-fn (conj args 'fn))))
    ;;
    ;; do
    ;;
    (= 'do type)
    (compile-do args)
    ;;
    ;; macros
    ;;
    ('#{cond clojure.core/cond
        when clojure.core/when
        when-not clojure.core/when-not
        -> clojure.core/->
        ->> clojure.core/->>
        } type)
    (compile-seq (macroexpand-1 form))
    ;;
    ;; if
    ;;
    (= 'if type)
    (compile-if args)
    ;;
    ;; let
    ;;
    ('#{let clojure.core/let} type)
    (compile-let args)
    ;;
    ;; special forms (||, + etc)
    ;;
    (special-forms type)
    (compile-special-form type args)
    ;;
    ;; must be
    ;;
    :default
    (compile-invoke form)
    ))

(defn compile-vector [v]
  (format "[%s]" (apply str (interpose ", " (map compile v)))))

(defn compile-map [m]
  (format "{%s}" (apply str (interpose ", " (map (fn [[k v]] (str (compile k) ": " (compile v))) m)))))

(defn compile [form]
  (cond
    (seq? form)
    (compile-seq form)
    (string? form)
    (pr-str form)
    (symbol? form)
    (compile-symbol form)
    (nil? form) "null"
    (vector? form)
    (compile-vector form)
    (map? form)
    (compile-map form)
    (keyword? form)
    (pr-str (name form))
    :default
    (str form)))

(defn print-copy [s]
  (println s)
  (spit "test.html" (format (slurp "test.html.template") s)))

(print-copy
  (compile '(console.log {:hi 3})))

(defn handler [{:keys [file]}]
  (println "handling" file)
  (let [
         [[_ ns] & forms] (read-file file)

         ]
    (spit-script "out" ns "hihi")))
