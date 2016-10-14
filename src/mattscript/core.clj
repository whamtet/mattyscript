(ns mattscript.core
  (:require [hawk.core :as hawk]))

(import java.io.File)

(def project-version (-> "project.clj" slurp read-string (nth 2)))

(defn spit-script [dir ns s]
  (let [
         elements (.split (str ns) "\\.")
         parent (apply str (interpose "/" (concat [dir] (butlast elements))))
         f (str parent "/" (last elements) ".js")
         ]
    (.mkdirs (File. parent))
    (spit f (format "//Compiled by Mattscript %s\n%s" project-version s))))

(defn read-file [f]
  (read-string (format "[%s]" (slurp f))))

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
    (hawk/watch! [{:paths paths
                   :filter (fn [_ {:keys [file]}] (and (.isFile file) (.endsWith (.getName file) ".clj")))
                   :handler handler2}])))

(declare handler)
(defonce watcher (safe-watcher ["src-mattscript"] #'handler))

(defn handler [{:keys [file]}]
  (println "handling" file)
  (let [
         [[_ ns] & forms] (read-file file)

         ]
    (spit-script "out" ns "hihi")))
