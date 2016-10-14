(ns mattyscript.core
  (:require [hawk.core :as hawk])
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

(defn interpose-lines [lines]
  (apply str (interpose "\n" (filter identity lines))))

(defn map-str [f & args]
  (apply str (apply map f args)))

(def special-symbols {"+" "_PLUS_" "?" "_QMARK_" "-" "_"})
(defn compile-symbol [symbol]
  (reduce (fn [s [a b]] (.replace s a b)) (str symbol) special-symbols))

(def special-forms '{+ " + " - " - " / " / " * " * " and " || " or " || "})
(defn compile-special-form [type args]
  (apply str
         (interpose (special-forms type) (map compile args))))

(declare compile-vector-arg compile-map-arg)
(defn compile-arg [parent-var i v]
  (cond
    (symbol? v) (format "var %s = %s[%s]\n" v parent-var i)
    (vector? v)
    (let [s (str (gensym))]
      (format "var %s = %s[%s]\n%s" s parent-var i (compile-vector-arg s v)))
    (map? v)
    (let [s (str (gensym))]
      (format "var %s = %s[%s]\n%s" s parent-var i (compile-map-arg s v)))
    :default
    (throw (Exception. (format "Impossible compile-arg %s %s %s" parent-var i v)))))

(defn compile-map-arg [parent-var {:keys [as strs] :as m}]
  (str
    (if as
      (format "var %s = %s\n" as parent-var))
    (apply str
           (for [s strs]
             (format "var %s = %s['%s']\n" s parent-var s)))
    (apply str
           (for [[k v] m :when (string? v)]
             (compile-arg parent-var v k)))))

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
                (after :as v)
                parent-var))
      (if (some #(= '& %) v)
        (format "var %s = %s.slice(%s)\n"
                (after '& v)
                parent-var
                (count normal-args))))))

(println (compile-vector-arg "parent" '[hi & rest :as y]))

(defn compile-arg-list [v]
  (format "var args = Array.from(arguments)\n%s" (compile-vector-arg "args" v)))

(defn compile-fn [[_ name arg-list & forms]]
  (let [
         [name arg-list forms]
         (if (vector? name)
           ["function" name (conj forms arg-list)]
           [name arg-list forms])
         ]
    (format "%s(){\n%s%s}" name (compile-arg-list arg-list) (map-str compile forms))))

(defn compile-seq [[type & args :as form]]
  (condp = type
    'def
    (let [[a b] args]
      (export-format form "var %s = %s\n" a (compile b)))
    'import
    (let [[path _ imports] args
          args (apply str (interpose ", " imports))]
      (export-format form "import { %s } from '%s'\n" args path))
    'class
    (let [[name superclass & methods] args]
      (export-format form "class %s extends %s {\n\n%s}" name superclass (map-str compile-fn methods)))
    'fn
    (compile-fn form)
    'defn
    (let [[name] args]
      (export-format form "var %s = %s\n" name (compile-fn methods)))
    ;default
    (cond
      (special-forms type)
      (compile-special-form type args)
      :default (str form)
      )))

(defn compile [form]
  (cond
    (seq? form)
    (compile-seq form)
    (string? form)
    (pr-str form)
    (symbol? form)
    (compile-symbol form)
    :default
    (str form)))

(defn handler [{:keys [file]}]
  (println "handling" file)
  (let [
         [[_ ns] & forms] (read-file file)

         ]
    (spit-script "out" ns "hihi")))
