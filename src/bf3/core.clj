(ns bf3.core
  (:use [ring.adapter.jetty]
        [compojure.core]
        noir.core
        hiccup.core
        hiccup.page-helpers
        [bf3.bf3stats :only [random-loadout]]
        [bf3.db :only [get-ts-users get-bl-users]]
        [bf3.stats :only [get-stats battleday-roster get-battleday]]
        [bf3.ki :only [ki-players get-ta-info get-ki-info]]
        [cheshire.core :only [encode generate-string]])
  (:require (compojure [route :as route])
            (ring.util [response :as response])
            (ring.middleware [multipart-params :as mp])
            [noir.response :as res]
            [clojure.string :as s]
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
  (GET  "/favicon.ico" [] "")

  (GET  "/kit/" [] (kit-wrapper (random-loadout)))
  (GET  "/kit/:player" [player] (kit-wrapper (random-loadout player)))

  (GET  "/gc/" [] (layout "GC stuff"))

  (GET  "/gc/ts-users.json" [] (res/json (get-ts-users)))
  (GET  "/gc/ts-stats.json" [] (res/json (get-stats (get-ts-users))))
  (GET  "/gc/ts-stats/:player.json" [player]
    (res/json (first (filter (fn [[key]] (= key player)) (get-stats (get-ts-users))))))

  (GET  "/gc/bl-users.json" [] (res/json (get-bl-users)))
  (GET  "/gc/bl-stats.json" [] (res/json (get-stats (get-bl-users))))
  (GET  "/gc/bl-stats/:player.json" [player]
    (res/json (first (filter (fn [[key]] (or (some #(= key (:personaId %))
                                                  (bl/get-user player))
                                            (= key player)))
                             (get-stats (get-bl-users))))))

  (GET  "/gc/roster" []
    (let [roster (battleday-roster (get-stats (get-bl-users)))]
                  (layout [:h1 "roster for battleday " (get-battleday)]
                          (into [:div#roster]
                                (for [server roster]
                                     (list [:h2 (str "server " (->> (filter #(= (last %) (->> server :server)) bl/server-ids)
                                                                    first key))]
                                           (->> (:users server)
                                                (map #(if (get-ki-info %)
                                                       (hash-map :origin-name % :army "KI")
                                                       (if (get-ta-info %)
                                                           (hash-map :origin-name % :army "TA")
                                                           (hash-map :origin-name % :army "HiT (not ki or ta)")
                                                           )))
                                                (sort-by :army)
                                                (partition-by :army)
                                                reverse
                                                (map (fn [army]  (list [:h3 (->> army first :army)]
                                                                      (->> army
                                                                           (map #(:origin-name %) )
                                                                           (interpose "<br/>"))))))))))))

  (GET  "/gc/update" [] (layout (do (bl/save-live-users)
                                    (ts/save-live-users))))

  (route/not-found "no here"))

(def my-app
      public-routes)


(defn -main [& m]
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8081"))]
    (doto (Thread. #(run-jetty #'my-app {:port port})) .start)))
