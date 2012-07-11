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
    [:p "use bf3.herokuapp.com/YOURSOLDIER?pdw=true&shotguns=true"
     "if you whant to use pdw and/or shoutguns"]
    [:p "you need to manually call an update on bf3stats.com"]]
   [:div#content content]
   [:div#todo [:b "todo"]
    (unordered-list ["design ;)"
                     [:div {:style "text-decoration: line-through;"}
                      "fix attachments on PDWs (some have attachments on diferent slots then the main weapons)"]
                     "vehicles" "configuration:"
                     (unordered-list ["classes"
                                      "prefere weapons/classes without 1/5 service star(s)"
                                      "prefere weapons with unlocked attachments"
                                      [:div {:style "text-decoration: line-through;"} "PWDs"]])

                     "add update call to bf3stats"
                     "squad loadouts"])]
   [:div#fotter
    [:p "data from bf3stats.com"]]))

(defpartial stalking-layout [platoones & content]
    (layout  [:style {:type "text/css"} (gaka/css [:div#servers
                                                   :background-color "#E5E5E5"
                                                   :height "auto"
                                                   :padding "10px"
                                                   :max-width "900px"
                                                   :border-radius "10px"
                                                   :border "1px solid #BBBBBB"
                                                  :margin "5px auto"
                                                  [:small :display "block"
                                                   [:a :color "#666666"
                                                     :text-decoration "none"]
                                                    [:a:hover :color "#333333"]]
                                                  [:div.server :clear "both"
                                                   :background-color "#EEEEEE"
                                                   :height "auto"
                                                   :padding "5px"
                                                   :max-width "100%"
                                                   :border-radius "10px"
                                                   :border "1px solid #999999"
                                                   :margin "5px auto"
                                                   [:h4 :font "normal 14px arial"
                                                    :margin-left "5px"
                                                    [:a :color "#333333"
                                                     :text-decoration "none"
                                                     :font-weight "bold"]
                                                    [:a:hover :color "#666666"]
                                                    [:span :margin "3px"
                                                     :font-size "12px"]
                                                    [:span.preset :font-style "italic"]
                                                    [:span.full :text-decoration "line-through"]]
                                                   [:div.user :background-color "#C5C5C5"
                                                    :height "64px"
                                                    :padding "5px"
                                                    :width "200px"
                                                    :border-radius "10px"
                                                    :border "1px solid #888888"
                                                    :margin "5px"
                                                    :float "left"
                                                    [:img :border-radius "10px"]]
                                                   [:div.user>* :float "left" :display "block"]
                                                   [:div.user>a :height "auto"
                                                    :text-align "center"
                                                    :padding "20px 0 0 0"
                                                    :margin "5px 0"
                                                    :width "135px"
                                                    :text-decoration "none"
                                                    :font-weight "bold"
                                                    :color "#666666"]
                                                   [:div.user>a:hover :color "#333333"]]]
                                                   [:div.clearer :clear "both"])])
    [:div#servers content
     (->> platoones  bl/get-playing-users
          (map (fn [[id s]]
                 (hash-map :server-id id
                           :server-name (->> s :server :server :name)
                           :player-numbers (select-keys (->> s :server :server)
                                                        [:maxPlayers :numPlayers])
                           :server-preset (bl/serverPrests (get-in s [:server :server :preset]))
                           :users (->> s :users
                                       (map #(select-keys (:user %)
                                                          [:username :gravatarMd5 :userId ]))
                                       distinct))))
          (sort #(compare (count (:users %2)) (count (:users %1))))
          (map #(list [:div.server [:h4 (link-to (bl/get-server-url (:server-id %))
                                                 (s/escape (:server-name %) {\< \>}))
                                    [:span.info  "("
                                     ((fn [{:keys [numPlayers maxPlayers]
                                           :or [numPlayers 0 maxPlayers 0]}]
                                        (if (=  maxPlayers numPlayers)
                                          [:span.players.full
                                           numPlayers "/" maxPlayers ]
                                          [:span.players
                                           numPlayers "/" maxPlayers ]))
                                      (:player-numbers %))
                                     [:span.preset (:server-preset %)]
                                     ")"]]
                       [:div#users
                        (map (fn [u] [:div.user
                                     [:img {:src (str "http://gravatar.com/avatar/"
                                                      (:gravatarMd5 u) "?s=64&d=mm")}]
                                     [:a {:href
                                          (str "http://battlelog.battlefield.com/bf3/user/"
                                               (:username u) "/")} (:username u)]])
                             (:users %))]
                       [:div.clearer]])))])

(defpage  "/" [] (layout "BF3 Stuff"))
(defpage  "/favicon.ico" [] "")

(defpage "/kit" []
  (response/redirect "/kit/"))
(defpage  "/kit/" [] (kit-wrapper (random-loadout)))
(defpage  "/kit/:player" {player :player pdw :pdw shotguns :shotguns kit :class}
  (kit-wrapper (random-loadout player :pdw pdw :shotguns shotguns :kit kit)))

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

(defpage "/gc/stalking" []
  (response/redirect "/stalking/gc"))

(defpage "/play-with-gc" []
  (response/redirect "/stalking/gc"))

(defpage "/stalking/" []
  (response/redirect "/stalking/gc"))

(defpage "/stalking" []
  (response/redirect "/stalking/gc"))

(defpage "/stalking/gc" []
  (stalking-layout
   (vals bl/gc-platoones)
   [:h1 "Play with your fellow GC soldiers"]
   [:small "Shows players in the GC platoons, "
    (link-to "http://battlelog.battlefield.com/bf3/platoon/2832655391300768492/" "Platoon 1")
    " "
    (link-to "http://battlelog.battlefield.com/bf3/platoon/2832655391533545956/" "Platoon 2")
    ". "]
   [:small
    (link-to "http://www.global-conflict.org/viewtopic.php?f=3&t=16475" "How to sign up for the platoons")]
   [:small "Inspired by " (link-to "https://stalkdice.ep.io/" "stalk dice")]
    ))

(defpage "/stalking/dice" []
  (stalking-layout
   (vals bl/dice-platoones)
    [:small "Inspired by " (link-to "https://stalkdice.ep.io/" "stalk dice")]
    ))

(defonce server (atom nil))

(defn -main [& m]
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8081"))]
    (reset! server (server/start port))))
