(ns mattyscript.watch
  (:require [hawk.core :as hawk]
            [mattyscript.core :as core]
            ))

(import java.io.File)

(def project-version (-> "project.clj" slurp read-string (nth 2)))

(defn read-file [f]
  (read-string (format "[%s]" (slurp f))))

(defn spit-script [dir ns target-index? s]
  (let [
         ns (.replace (str ns) "-" "_")
         elements (.split ns "\\.")
         parent (if target-index?
                  (apply str (interpose "/" (concat [dir] elements)))
                  (apply str (interpose "/" (concat [dir] (butlast elements)))))
         f (if target-index?
             (str parent "/index.js")
             (str parent "/" (last elements) ".js"))
         ]
    (.mkdirs (File. parent))
    (spit f (format "//Compiled by Mattyscript %s\n%s" project-version s))))

(defn subdirs [f]
  (filter #(.isDirectory %) (file-seq (File. f))))

(defn src-files [f]
  (filter #(.endsWith (.getName %) ".clj") (file-seq (File. f))))

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
    ;warmup compile
    (doseq [f (mapcat src-files paths)]
      (handler {:file f}))
    (hawk/watch! [{:paths (mapcat subdirs paths)
                   :filter (fn [_ {:keys [file kind]}] (and (= :modify kind) (.isFile file) (.endsWith (.getName file) ".clj")))
                   :handler handler2}])))

(defn make-handler [out]
  (fn handler [{:keys [file]}]
    (println "handling" file)
    (let [
           [[_ ns & ns-opts] & forms] (read-file file)
           target-index? (some #(and (coll? %) (= :index (first %))) ns-opts)
           ]
      (spit-script out ns target-index? (apply str (interpose "\n" (map #_core/expand-compile core/rename-compile forms)))))))

(defonce watcher (safe-watcher ["../src-mattyscript"] (make-handler "../taipan-react/src/components/")))
