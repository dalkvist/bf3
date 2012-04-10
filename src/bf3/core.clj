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
            [noir.server :as server]
            [clojure.string :as s]
            [gaka [core :as gaka]]
            [bf3.ts :as ts]
            [bf3.bl :as bl]))

(defpartial layout [ & content]
  (html5
   [:head
    [:title "Battlefield 3" ]
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


(defpage  "/" [] (layout "BF3 Stuff"))
(defpage  "/favicon.ico" [] "")

(defpage "/kit" []
  (response/redirect "/kit/"))
(defpage  "/kit/" [] (kit-wrapper (random-loadout)))
(defpage  "/kit/:player" [player] (kit-wrapper (random-loadout player)))

(defpage "/gc" []
  (response/redirect "/gc/"))
(defpage  "/gc/" [] (layout "GC stuff"))

(defpage  "/gc/ts-users.json" [] (res/json (get-ts-users)))
(defpage  "/gc/ts-stats.json" [] (res/json (get-stats (get-ts-users))))
(defpage  "/gc/ts-stats/:player.json" [player]
  (res/json (first (filter (fn [[key]] (= key player)) (get-stats (get-ts-users))))))

(defpage  "/gc/bl-users.json" [] (res/json (get-bl-users)))
(defpage  "/gc/bl-stats.json" [] (res/json (get-stats (get-bl-users))))
(defpage  "/gc/bl-stats/:player.json" [player]
  (res/json (first (filter (fn [[key]] (or (some #(= key (:personaId %))
                                                (bl/get-user player))
                                          (= key player)))
                           (get-stats (get-bl-users))))))

(defpage  "/gc/roster" []
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

(defpage  "/gc/update" [] (layout (do (bl/save-live-users)
                                  (ts/save-live-users))))

(defpage "/play-with-gc" []
  (response/redirect "/gc/stalking"))

(defpage "/gc/stalking" []
  (layout  [:ul (->> (vals bl/platoones) bl/get-playing-users
                     (map (fn [[id s]] (hash-map :server-id id
                                                :server-name (->> s :server :server :name)
                                                :server-preset (bl/serverPrests (get-in s [:server :server :preset]))
                                                :users (->> s :users (map #(select-keys (:user %)
                                                                                        [:username :gravatarMd5 :userId ]))))))
                     (map #(list [:li [:h4 (link-to (bl/get-server-url (:server-id %)) (:server-name %))
                                       [:span.preset (str "(" (:server-preset %) ")")]]
                                  [:div.users "Users: " (map (fn [u] [:div.user [:img {:src (str "http://gravatar.com/avatar/"
                                                                                         (:gravatarMd5 u)
                                                                                         "?s=32&d=mm")}]
                                                              (:username u)])
                                                      (:users %))]]))
                     )]))

(defonce server (atom nil))

(defn -main [& m]
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8081"))]
    (reset! server (server/start port))))
