(ns mattyscript.watch
  (:require [hawk.core :as hawk]
            [mattyscript.core :as core]
            ))

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

(defn handler [{:keys [file]}]
  (println "handling" file)
  (let [
         [[_ ns] & forms] (read-file file)
         ]
    (spit-script "out" ns "hihi")))
