(ns mattyscript.analyze)

(def forms
  (read-string
    (format "(%s)"
            (slurp "src/mattyscript/core.clj"))))

(defn load-form [form]
  (if (and (coll? form) (= 'defn (first form)))
    [(second form) (flatten (drop 3 form))]))

(def defn-forms
  (filter identity (map load-form forms)))

(def nodes (set (map first defn-forms)))
(def edge-map
  (into {}
        (for [[k v] defn-forms]
          [k (filter nodes v)])))

(use 'rhizome.viz)

(view-graph nodes edge-map
            :node->descriptor (fn [n] {:label n}))
