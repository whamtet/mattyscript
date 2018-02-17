(ns mattyscript.watch
  (:require [hawk.core :as hawk]
            [mattyscript.core :as core]
            [mattyscript.refresher :as refresher])
  (:import java.io.File
           java.nio.file.Files
           java.nio.file.Paths))

(def project-version (-> "project.clj" slurp read-string (nth 2)))

(defn read-file [f]
  (read-string (format "[%s]" (slurp f))))

(defn spit-script [dir ns target-index? s suffix]
  (let [
         ns (.replace (str ns) "-" "_")
         elements (.split ns "\\.")
         parent (if target-index?
                  (apply str (interpose "/" (concat [dir] elements)))
                  (apply str (interpose "/" (concat [dir] (butlast elements)))))
         f (if target-index?
             (str parent "/index" suffix)
             (str parent "/" (last elements) suffix))
         ]
    (.mkdirs (File. parent))
    (spit f (format "//Compiled by Mattyscript %s\n%s" project-version s))))

(defn subdirs [f]
  (filter #(.isDirectory %) (file-seq (File. f))))

(defn src-files [f]
  (filter #(.endsWith (.getName %) ".cljs") (file-seq (File. f))))

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
    (doseq [f (sort (mapcat src-files paths))]
      (handler {:file f}))
    (hawk/watch! [{:paths (mapcat subdirs paths)
                   :filter (fn [_ {:keys [file kind]}] (and (= :modify kind) (.isFile file) (.endsWith (.getName file) ".cljs")))
                   :handler handler2}])))

(defn ->path [s]
  (Paths/get s (make-array String 0)))

(defn safe-watcher-files
  "safe-watcher for a set of files"
  [files handler]
  (let [paths (map ->path files)]
    (hawk/watch! [{:paths [(-> files first File. .getParent)]
                   :filter (fn [_ {:keys [file]}]
                             (let [path (-> file .getAbsolutePath ->path)]
                               (some #(Files/isSameFile path %) paths)))
                   :handler handler}])))

(defn src-handler [out suffix]
  (fn handler [{:keys [file]}]
    (println "handling" file)
    (let [
           [[_ ns & ns-opts] & forms] (read-file file)
           target-index? (some #(and (coll? %) (= :index (first %))) ns-opts)
           ]
      (spit-script out ns target-index? (apply str (interpose "\n" (map core/rename-compile forms))) suffix))))

(defn parse-args [args]
  (loop [todo args
         done {:compile [] :outputs [] :suffix ".jsx"}]
    (if-let [flag (first todo)]
      (case flag
        "--compile" (recur (drop 3 todo) (update done :compile conj (take 2 (rest todo))))
        "--suffix" (recur (drop 2 todo) (assoc done :suffix (second todo)))
        "--output" (recur (drop 2 todo) (update done :outputs conj (second todo))))
      done)))

(defn -main [& args]
  (let [{:keys [compile outputs suffix]} (parse-args args)]
    (doseq [[src target] compile]
      (println "watcher from" src "to" target)
      (safe-watcher [src] (src-handler target suffix)))
    (safe-watcher-files outputs refresher/notify!)))
