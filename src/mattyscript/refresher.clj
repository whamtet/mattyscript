(ns mattyscript.refresher
  (:require [org.httpkit.server :as httpkit]))

(def channel (atom nil))
(defn notify! [& _]
  (when-let [ch @channel]
    (httpkit/send! ch "refresh")))

(defmacro with-open-cors-channel
  "Opens an http streaming response with the correct headers for cors.
  Return partial string responses by calling (httpkit/send! \"some string\" false).
  No middleware is applied."
  [req channel & body]
  `(httpkit/with-channel ~req ~channel
     (try
       (httpkit/send! ~channel
                      {:status 200
                       ;if you send an empty string it closes the channel!
                       :body " "
                       :headers {"Access-Control-Allow-Origin" "*"}} false)
       ~@body)))

(defn handler [{:keys [websocket?] :as req}]
  (if websocket?
    (with-open-cors-channel req ch
      (reset! channel ch))
    {:status 200
     :headers {"Content-Type" "application/javascript"}
     :body "window.ws = new WebSocket('ws://localhost:5565');
     ws.onmessage = function(x) {if (x.data == 'refresh') {location.reload()}}"}))

(defonce server (httpkit/run-server #'handler {:port 5565}))
