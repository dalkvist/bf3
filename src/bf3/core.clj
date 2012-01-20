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
               [:title "Battlefield 3 loadout randomizer"]
               ;; (include-css "/css/reset.css")
               ]
              [:body
               [:div#wrapper
                [:div#alphaInfo
                 [:b "Simple battlefield 3 loadout randomizer"]
                 [:p "use bf3.herokuapp.com for random loadout with all unlocks, inc all attachments"]
                 [:p "or use bf3.herokuapp.com/YOURSOLDIER "
                  "for a loadout with only the stuff you have unlocked"]
                 [:p "you need to manually call an update on bf3stats.com"]]
                [:div#content content]]
               [:div#todo [:b "todo"]
                (unordered-list ["design ;)"
                                 "fix attachments on PDWs (some have attachments on diferent slots then the main weapons)"
                                 "vehicles" "configuration:"
                                 (unordered-list ["classes"
                                                  "prefere weapons/classes without 1/5 service star(s)"
                                                  "prefere weapons with unlocked attachments"
                                                  "add update call to bf3stats"
                                                  "vehicle loadouts"
                                                  "squad loadouts"])])]
               [:div#fotter 
                 [:p "data from bf3stats.com"]]]))


(defroutes public-routes
  (GET  "/" [] (layout (random-loadout)))
  (GET  "/:player" [player] (layout (random-loadout player)))
  (route/not-found "no here"))

(def my-app
      public-routes)


(defn server []
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8081"))]
    (doto (Thread. #(run-jetty #'my-app {:port port})) .start)))

