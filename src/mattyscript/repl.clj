(ns mattyscript.repl
  (:require [mattyscript.core :as core]
            [clj-tcp.client :as client]
            [clojure.core.async :as async :refer [go <! go-loop]]))

(defn read-print-ch [{ch :read-ch}]
  (go
    (loop [ch1 ch]
      (if-let [c (<! ch1)]
        (if (instance? clj_tcp.client.Reconnected c)
          (do
            (recur (-> c :client :read-ch)))
          (do
            (println (String. c))
            (recur ch1)))))))

(defn read-error-ch [c]
  (go-loop []
           (when-let [[e v] (<! (:error-ch c))]
             (prn "Error " e)
             (recur))))

(defn read-internal-error-ch [c]
  (go-loop []
           (when-let [[e v] (<! (:internal-error-ch c))]
             (prn "Internal Error " e)
             (recur))))

(defonce c (doto (client/client "localhost" 3021 {})
             read-print-ch
             read-error-ch
             read-internal-error-ch
             (client/write! (.getBytes "\n"))))

(defn repl []
  (.flush *out*)
  (let [s (.trim (read-line))]
    (case s
      "" (recur)
      "exit" nil
      (do
        (client/write! c (-> s read-string core/rename-compile (str "\n") .getBytes))
        (recur)))))
