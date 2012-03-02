(ns bf3.core
  (:use [ring.adapter.jetty]
        [compojure.core]
        noir.core
        hiccup.core
        hiccup.page-helpers
        [bf3.bf3stats :only [random-loadout]]
        [bf3.db :only [get-ts-users]]
        [bf3.db :only [get-bl-users]]
        [cheshire.core :only [encode generate-string]])
  (:require (compojure [route :as route])
            (ring.util [response :as response])
            (ring.middleware [multipart-params :as mp])
            [noir.response :as res]
            [bf3.ts :as ts]
            [bf3.bl :as bl]))

(defpartial layout [& content]
            (html5
             [:head
              [:title "Battlefield 3 loadout randomizer"]
              ;; (include-css "/css/reset.css")
              ]
             [:body
              [:div#wrapper
               content
               ]]))

(defpartial kit-wrapper [& content]
  (layout
   [:div#alphaInfo
    [:b "Simple battlefield 3 loadout randomizer"]
    [:p "use bf3.herokuapp.com for random loadout with all unlocks, inc all attachments"]
    [:p "or use bf3.herokuapp.com/YOURSOLDIER "
     "for a loadout with only the stuff you have unlocked"]
    [:p "you need to manually call an update on bf3stats.com"]]
   [:div#content content]
   [:div#todo [:b "todo"]
    (unordered-list ["design ;)"
                     "fix attachments on PDWs (some have attachments on diferent slots then the main weapons)"
                     "vehicles" "configuration:"
                     (unordered-list ["classes"
                                      "prefere weapons/classes without 1/5 service star(s)"
                                      "prefere weapons with unlocked attachments"
                                      "PWDs"])

                     "add update call to bf3stats"
                     "squad loadouts"])]
   [:div#fotter
    [:p "data from bf3stats.com"]]))


(defroutes public-routes
  (GET  "/" [] (layout "BF3 Stuff"))
  (GET  "/kit/" [] (kit-wrapper (random-loadout)))
  (GET  "/favicon.ico" [] "")
  (GET  "/kit/:player" [player] (kit-wrapper (random-loadout player)))
  (GET  "/gc/" [] (layout "GC stuff"))
  (GET  "/gc/ts-users.json" [] (res/json (get-ts-users)))
  (GET  "/gc/bl-users.json" [] (res/json (generate-string (get-bl-users))))
  (GET  "/gc/update" [] (layout (do (bl/save-live-users)
                                    (ts/save-live-users))))
  (route/not-found "no here"))

(def my-app
      public-routes)


(defn -main [& m]
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8081"))]
    (doto (Thread. #(run-jetty #'my-app {:port port})) .start)))
