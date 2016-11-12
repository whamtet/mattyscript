(ns mattyscript.core
  (:require [clojure.walk :as walk]
            )
  (:refer-clojure :exclude [compile]))

(declare compile)

(defn export-format [form s & args]
  (let [
         {:keys [export default]} (meta form)
         prefix
         (cond
           (and export default) "export default "
           export "export ")
         ]
    (str prefix (apply format s args))))

(defn map-str [f & args]
  (apply str (apply map f args)))

(def special-symbols {"+" "_PLUS_"
                      "!" "_BANG_"
                      "?" "_QMARK_"
                      "-" "_"
                      "#" "_HASH_"})

(defn compile-symbol [symbol]
  ('{class "claz"} symbol
           (reduce (fn [s [a b]] (.replace s a b)) (str symbol) special-symbols)))

(def special-forms '{+ " + " - " - " / " / " * " * " and " && " or " || " not= " != " = " == "
                     > " > " >= " >= " <= " <= " < " < " mod " % "
                     })

(defn compile-special-form [type args]
  (let [p #(if (coll? %) (format "(%s)" (compile %)) (compile %))]
    (if (and (= '- type) (= 1 (count args)))
      (str "- " (compile (first args)))
      (format "(%s)"
              (apply str
                     (interpose (special-forms type) (map p args)))))))

(defn compile-import [[path imports]]
  (let [
         imports2 (apply str (interpose ", " (map compile-symbol imports)))
         check "" ;(map-str #(format "if (!%s) throw new Error('import failure ' + %s)\n" % %) imports)
         ]
    (format "import { %s } from '%s';" imports2 path)))

;;
;; Destructure variable binding
;;

(declare compile-vector-arg compile-map-arg)
(defn compile-arg [parent-var i v]
  (cond
    (symbol? v) (format "var %s = %s[%s];\n" (compile-symbol v) parent-var i)
    (vector? v)
    (let [s (str (gensym))]
      (format "var %s = %s[%s];\n%s" s parent-var i (compile-vector-arg s v)))
    (map? v)
    (let [s (str (gensym))]
      (format "var %s = %s[%s];\n%s" s parent-var i (compile-map-arg s v)))
    :default
    (throw (Exception. (format "Impossible compile-arg %s %s %s" parent-var i v)))))

(defn compile-let-arg [[k v]]
  (cond
    (symbol? k) (format "var %s = %s;\n" (compile-symbol k) (compile v))
    (vector? k)
    (let [s (str (gensym))]
      (format "var %s = %s;\n%s" s (compile v) (compile-vector-arg s k)))
    (map? k)
    (let [s (str (gensym))]
      (format "var %s = %s;\n%s" s (compile v) (compile-map-arg s k)))
    :default
    (throw (Exception. (format "Impossible let-arg %s %s" k v)))))

(defn compile-let-args [binding-vector]
  (map-str compile-let-arg (partition 2 binding-vector)))

(defn compile-map-arg [parent-var {:keys [as strs] :as m}]
  (str
    (if as
      (format "var %s = %s;\n" (compile-symbol as) parent-var))
    (apply str
           (for [s strs :let [s2 (compile-symbol s)]]
             (format "var %s = %s['%s'];\n" s2 parent-var s)))
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
  #_(format "var args = Array.from(arguments)\n%s" (compile-vector-arg "args" v))
  (format "var args = [].slice.call(arguments)\n%s" (compile-vector-arg "args" v)))

;;
;; end Destructure
;;

(defn map-last [f1 f2 s]
  (if (not-empty s)
    (concat (map f1 (butlast s)) [(f2 (last s))])))

(defn do-statements [statements]
  (apply str
         (map-last
           #(str (compile %) ";\n")
           #(str "return " (compile %) ";\n")
           statements)))

(defn compile-fn [[_ name arg-list & forms]]
  (let [
         [name arg-list forms]
         (if (vector? name)
           ["function" name (conj forms arg-list)]
           [name arg-list forms])
         simple-binding? #(and (not= '& %) (symbol? %))
         do-statements (if (= 'constructor name) (apply str (map #(str (compile %) ";\n") forms)) (do-statements forms))
         ]
    (if (every? simple-binding? arg-list)
      (format "%s(%s){\n%s}\n" name (apply str (interpose ", " (map compile-symbol arg-list))) do-statements)
      (format "%s(){\n%s%s}\n" name (compile-arg-list arg-list) do-statements))))

(defn compile-do [statements]
  (format "(function(){%s}())" (do-statements statements)))

(defn compile-invoke [form]
  (let [
         form (map compile form)
         [method obj & method-args] form
         method (str method)
         [f & args] form
         ]
    (cond
      (.startsWith method "._")
      (format "%s.%s" obj (.substring method 2))
      (.startsWith method ".")
      (format "%s%s(%s)" obj method (apply str (interpose ", " method-args)))
      :default
      (format "%s(%s)" f (apply str (interpose ", " args))))))

(defn compile-new [args]
  (str "new " (compile-invoke args)))

(defn compile-if [[cond then else]]
  (if else
    (format "(function() {if (%s) {return %s} else {return %s}}())" (compile cond) (compile then) (compile else))
    (format "(function() {if (%s) {return %s}}())" (compile cond) (compile then))))

(defn compile-let [[binding-vector & body]]
  (format "(function() {\n%s%s}())" (compile-let-args binding-vector) (do-statements body)))

(defn compile-get [[m k alt]]
  (if alt
    (format "(%s[%s] || %s)" (compile m) (compile k) (compile alt))
    (format "%s[%s]" (compile m) (compile k))))

(defn compile-get-in [[m v alt]]
  (let [
         prefix (apply str (compile m) (map #(format "[%s]" (compile %)) v))
         ]
    (if alt
      (format "(%s || %s)" prefix (compile alt))
      prefix)))

(defn compile-assoc-in [[m v value]]
  (format "%s%s = %s" (compile m) (map-str #(format "[%s]" (compile %)) v) (compile value)))

;;
;; for and doseq
;;
(defn parse-bindings [v]
  (let [
         v (partition 2 v)
         ]
    (loop [
            done []
            curr {}
            todo v
            ]
      (if-let [[k v] (first todo)]
        (cond
          (keyword? k)
          (recur done (assoc curr k v) (next todo))
          (empty? curr)
          (recur done {:bindings k :seq v} (next todo))
          :default
          (recur (conj done curr) {} todo))
        (conj done curr)))))

(defn named-format [s m]
  (reduce (fn [s [k v]] (.replace s (str k) (str v))) s m))
(defmacro keyzip [& syms]
  `(zipmap ~(mapv keyword syms) ~(vec syms)))

(defn compile-ring [array [{:keys [bindings seq while when] sublet :let} & rings] body]
  (let [
         parent-var (gensym "parent_")
         index-var (gensym "index_")
         parent-binding (format "var %s = %s;\n" parent-var (compile seq))
         sublet (concat [bindings `(~'get ~parent-var ~index-var)] sublet)
         sublet (compile-let-args sublet)
         while-clause (if while (throw (Exception. "while-clause unimplemented")) #_(format "if (!(%s)) break;\n" (compile while)))
         when-clause (if when (format "if (!(%s)) return;\n" (compile when)))
         body (cond
                (not-empty rings) (compile-ring array rings body)
                array (format "%s.push(%s);\n" array (compile body))
                :default (apply str (interpose ";\n" (map compile body))))
         ]
    (named-format ":parent-bindingfor(var :index-var = 0; :index-var < :parent-var.length; :index-var++) {
                  (function() {:sublet:while-clause:when-clause:body}())}"
                  (keyzip parent-binding parent-var index-var sublet body while-clause when-clause))))


(defn compile-doseq [[bindings & body]]
  (format "(function() {%s}())" (compile-ring nil (parse-bindings bindings) body)))

(defn compile-for [[bindings body]]
  (let [array (str (gensym "array_"))]
    (format "(function() {
            var %s = []
            %s
            return %s}())" array (compile-ring array (parse-bindings bindings) body) array)))
;;
;; end for, doseq
;;

(defn compile-apply [args]
  (let [
         [f & args] (map compile args)
         ]
    (if (empty? args)
      (format "%s()" f)
      (let [
             temp-var (gensym "temp_")
             unshift-args (apply str (interpose ", " (butlast args)))
             ]
        (format "(function() {var %s = %s
                %s.unshift(%s)
                return %s.apply(null, %s)}())" temp-var (last args) temp-var unshift-args f temp-var)))))

(defn compile-throw [[error]]
  (format "(function() {throw %s}())" (compile error)))

(defn compile-try [args]
  (let [
         body (do-statements (butlast args))
         [_ e & catch-statements] (last args)
         catch-statements (do-statements catch-statements)
         ]
    (format "(function() {try { %s } catch(%s) { %s }}())" body e catch-statements)))

(defn compile-json= [args]
  (compile `(~'= ~@(map #(list 'JSON.stringify %) args))))

(defn compile-seq [[type & args :as form]]
  ;(println "compile-seq" form)
  (cond
    ;;
    ;; json=
    ;;
    (= 'json= type)
    (compile-json= args)
    ;;
    ;; try
    ;;
    (= 'try type)
    (compile-try args)
    ;;
    ;; new
    ;;
    (= 'new type)
    (compile-new args)
    ;;
    ;; def
    ;;
    (= 'def type)
    (let [[a b] args]
      (export-format a "var %s = %s\n" (compile a) (compile b)))
    ;;
    ;; set
    ;;
    (= 'set! type)
    (let [[a b] args]
      (format "%s = %s\n" (compile a) (compile b)))
    ;;
    ;; import
    ;;
    (= 'import type)
    (compile-import args)
    ;;
    ;; class
    ;;
    (= 'class type)
    (let [[name superclass & methods] args]
      (export-format name "class %s extends %s {\n\n%s}" name superclass (map-str compile-fn methods)))
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
      (export-format name "var %s = %s\n" (compile name) (compile-fn (conj args 'fn))))
    ;;
    ;; do
    ;;
    (= 'do type)
    (compile-do args)
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
    ;; nil?
    ;;
    ('#{nil? clojure.core/nil?} type)
    (format "null == (%s)" (compile (first args)))
    ;;
    ;; get, get-in, assoc-in
    ;;
    ('#{get clojure.core/get} type)
    (compile-get args)
    ('#{get-in clojure.core/get-in} type)
    (compile-get-in args)
    (= 'assoc-in type)
    (compile-assoc-in args)
    ;;
    ;; for, doseq
    ;;
    (= 'doseq type)
    (compile-doseq args)
    (= 'for type)
    (compile-for args)
    ;;
    ;; not
    ;;
    ('#{not clojure.core/not} type)
    (let [[x] args]
      (format "!(%s)" (compile x)))
    ;;
    ;; apply
    ;;
    (= 'apply type)
    (compile-apply args)
    ;;
    ;; literal
    ;;
    (= 'literal type)
    (first args)
    ;;
    ;; throw
    ;;
    (= 'throw type)
    (compile-throw args)
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

(defn symbolize [s]
  (let [s (name s)]
    (if (= s (.toLowerCase s)) s (symbol s))))

(defn compile-vector [[kw & rest :as v]]
  (if (keyword? kw)
    (compile-invoke (concat ['h (symbolize kw)] rest))
    (format "[%s]" (apply str (interpose ", " (map compile v))))))

(defn compile-map [m]
  (format "{%s}" (apply str (interpose ", " (map (fn [[k v]] (str (compile k) ": " (compile v))) m)))))

(defn compile-set [s]
  (format "{%s}" (apply str (interpose ", " (map #(let [s (compile %)] (str s ": " s)) s)))))

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
    (set? form)
    (compile-set form)
    (keyword? form)
    (pr-str (name form))
    :default
    (str form)))

(defn macroexpand-special [form]
  (if (seq? form)
    (let [
           [f arg] form
           ]
      (cond
        ('#{cond clojure.core/cond
            when clojure.core/when
            when-not clojure.core/when-not
            -> clojure.core/->
            ->> clojure.core/->>
            if-not clojure.core/if-not
            some-> clojure.core/some->
            if-let clojure.core/if-let
            when-let clojure.core/when-let
            doto clojure.core/doto
            } f)
        (macroexpand-special (macroexpand-1 form))
        (.endsWith (str f) ".") (macroexpand-1 form)
        (= 'defmacro f) (str (eval form))
        (= 'expand f) (macroexpand arg)
        :default form))
    form))

(defn expand-compile [form]
  (compile (walk/prewalk macroexpand-special form)))

(defn print-copy [s]
  (println s)
  (spit "test.html" (format (slurp "test.html.template") s)))

(defn read-file [f]
  (read-string (format "[%s]" (slurp f))))

(defn compile-file [f]
  (apply str (interpose "\n" (map expand-compile (rest (read-file f))))))

;(spit "../taipan-preact/src/components/app.js" (compile-file "src-mattyscript/app.clj"))
