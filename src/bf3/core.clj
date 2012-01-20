(ns bf3.core
  (:use [ring.adapter.jetty]
        [compojure.core]
        noir.core
        hiccup.core
        hiccup.page-helpers
        [bf3.bf3stats :only [random-loadout]])
  
  (:require (compojure [route :as route])
            (ring.util [response :as response])
            (ring.middleware [multipart-params :as mp]))) 

(defpartial layout [& content]
            (html5
              [:head
               [:title "Battlefield 3"]
               ;; (include-css "/css/reset.css")
               ]
              [:body
               [:div#wrapper
                content]]))


(defroutes public-routes
  (GET  "/" [] (layout (random-loadout)))
  (GET  "/:player" [player] (layout (random-loadout player)))
  (route/not-found "no here"))

(def my-app
      public-routes)


(defn server []
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8081"))]
    (doto (Thread. #(run-jetty #'my-app {:port port})) .start)))

