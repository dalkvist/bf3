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
        [bf3.info :only [battle-info]]
        [cheshire.core :only [encode generate-string]])
  (:require (compojure [route :as route])
            (ring.util [response :as response])
            (ring.middleware [multipart-params :as mp])
            [noir.response :as res]
            [noir.request :as req]
            [noir.server :as server]
            [clojure.string :as s]
            [gaka [core :as gaka]]
            [bf3.ts :as ts]
            [bf3.bl :as bl]
            [clj-http.client :as client]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [clj-time.coerce :as time-cc]))


(def ^{:dynamic true} *short-cache-time* (* 59 1000))


(defn get-time-string [t]
  (->> (clj-time.format/parse t)
       (clj-time.format/unparse (clj-time.format/formatters :date-hour-minute))))

(defn get-interval-string [i]
  (-> i
      (.toPeriod (org.joda.time.PeriodType/yearWeekDayTime))
      (#(let [y (.getYears   %)
              w (.getWeeks   %)
              d (.getDays    %)
              h (.getHours   %)
              m (.getMinutes %)]
          (str
           (when (> y 0) (str y " Years"))
           (when (> w 0) (str " " w " weeks"))
           (when (> d 0) (str " " d " days"))
           (when (> h 0) (str " " h " hours"))
           (when (> m 0) (str " " m " minutes" )))))
      (s/trim)
      (#(if (empty? %)
          "~2 minutes"
          %))))


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


(def rank-url "http://battlelog-cdn.battlefield.com/cdnprefix/14d47a6313f99/public/profile/bf3/stats/ranks/tiny/")

(def map-preview-url "http://battlelog-cdn.battlefield.com/cdnprefix/9aa162d40ad4/public/base/bf3/map_images/30x21/")

(defn show-live-info [liveinfo]
  [:div#info
   [:style {:type "text/css"} (gaka/css [:#info
                                                [:.team :float "left"
                                                 :margin-right "25px"
                                                 :width "430px"
                                                 [:.score :float "right"
                                                  [:.max :float "left"
                                                   :font-size "12px"]
                                                  [:.current :float "right"
                                                   :font-weight "bold"
                                                   :font-size "18px"
                                                   :margin-top "-2px"]
                                                  [:.bar :display "inline-block"
                                                   :width "300px"
                                                   :height "6px"
                                                   :padding "1px"
                                                   :margin "3px 5px"
                                                   :border "1px solid #cccccc"
                                                   [:span :float "right"
                                                    :height "100%"
                                                    :background-color "#000000"]
                                                   [:.bleed
                                                    :background-color "rgba(255, 0, 0, 0.33)"]
                                                   [:.death
                                                    :background-color "rgba(200, 0, 0, 0.55)"]]]
                                                 [:.stats :clear :both
                                                  [:* :display "inline-block"]
                                                  [:span [:> [:* :margin "5px"]]
                                                   [:h5 :margin "0px 0px 5px 5px"]]]
                                                 [:tr :font-family "Arial"
                                                  [:th
                                                   :font-weight "normal"
                                                   :font-size "12px" ]
                                                  ["th:nth-child(3)" :text-align "left"
                                                   :padding-left "8px"]
                                                  [:td
                                                   :border-top "1px solid #F2F2F2"
                                                   :border-right "1px solid #F2F2F2"
                                                   :padding "0 5px"
                                                   :vertical-align "middle"
                                                   :text-align "center"]
                                                  [:span :margin "0 0 0 5px"]
                                                  [:img :margin "-2px 0 0 0"]
                                                  [:.tags :font-size "14px"]
                                                  ["td:nth-child(2)"
                                                   :background "#A8A7A6"
                                                   :color "white"
                                                   :font-weight "bold"
                                                   :font-size "12px"
                                                   :line-height "18px"
                                                   :margin "5px"
                                                   :padding "2px"
                                                   :width "16px"
                                                   :height "17px"
                                                   :display "inline-block"
                                                   :-webkit-border-radius "2px"
                                                   :-moz-border-radius "2px"
                                                   :border-radius "2px"]
                                                  ["td:nth-child(3)" :width "300px"
                                                   [:* :float "left"]]]]
                                                [:#t2 [:.score :float "left"
                                                       [:.max :float "right"]
                                                       [:.bar [:span :float "left"]]
                                                       [:.current :float "left"]]]] )]
   (for [team (partition-by :team (:users liveinfo))]
     [:div.team {:id (str "t" (name (:team (first team))))}
      ((fn [score]
         [:div.score
          [:span.max (:max score)]
          [:span.bar
           [:span {:style (str "width:" (- (float (* 100 (/ (:current score) (:max score)))) 1) "%;")}]
           [:span.death {:style (str "width:" (float (* 100 (/ (:bleed score) (:max score)))) "%;")}]
           [:span.bleed {:style (str "width:"  (float (* 100 (/ (:deaths score) (:max score)))) "%;")}]]
          [:span.current (:current score)]])
       (get (:stats liveinfo) (:team (first team)) {}))
      (into [:div.stats] (concat [[:h4 (char 0x03a3)]]
                                 (map #(vector :span {:class  (name (first %))} [:h5 (name (first %)) ":"]
                                               [:span (str (second %))])
                                      (merge (dissoc (get (:stats liveinfo) (:team (first team)))
                                                     :max :current)
                                             {:players (count (:users liveinfo))}))))
      [:table
       [:thead (into [:tr] (map #(vector :th %) ["#" "Sq" "Soldier Name" "K" "D" "Score"]))]
       (into [:tbody]
             (loop [users (reverse (sort-by :score team)) pos 1 res []]
               (if (empty? users)
                 res
                 (recur
                  (rest users)
                  (inc pos)
                  (conj res
                        (into [:tr]
                              (->> [ pos (char (+ 65 (:squad (first users))))
                                    (list (#(vector :img {:src (str rank-url (if (< 45 %) (str "ss" (- % 45))
                                                                                 (str "r" %))  ".png")
                                                          :height "23px" :width " 29px"})
                                           (:rank (first users)))
                                          [:span.tags (let [tags (:clanTags (first users))]
                                                        (when (not-empty tags) (str "[" tags "]")))]
                                          [:span.name (:personaName (first users))])
                                    (:kills (first users))
                                    (:deaths (first users)) (:score (first users))]
                                   (map #(vector :td  (if (coll? %) % (str %)))))))))))]])])

(defn show-battle-info [battle]
  [[:style {:type "text/css"} (gaka/css
                               [:.expansions :display "none"]
                               [:.duration :display "block"])]
   [:div (if-let [name (->> battle :server :info :name)]
           name
           (->> (:server battle) bl/server-info :server :name))]
    [:div.map [:img {:src (str map-preview-url (s/lower-case (:map battle)) ".jpg")}]
     [:span.name (bl/maps (:map battle))]
     [:span.mode (bl/mapModes (:mapMode battle))]
     [:span.variant (cond (= 1 (:mapVariant battle)) "Tiny (16 player)"
                          true "")]]
    [:div.time [:span.start [:span "start: "]
                (get-time-string (->> battle :time :start)) " utc"]
     [:span.end [:span "end: "] (get-time-string (->> battle :time :end)) " utc"]
     ;;TODO add relative to SBT
     [:span.duration [:span "duration: "]
      (->> (time/interval (clj-time.format/parse (->> battle :time :start))
                          (clj-time.format/parse (->> battle :time :end)))
           get-interval-string)]]])

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

(defn- get-battle [gameid]
  (->> (client/get (str "http://work.dalkvist.se:8081/get-battle/" gameid
                        "?host=" ((:headers (req/ring-request)) "host") )) :body))

(def battle (mem/memo-ttl get-battle *short-cache-time*))

(defpage "/battle/:gameid" [gameid]
  (html5 (battle gameid)))


(defpage "/get-battle/:gameid" {:keys [gameid start end host] :or {start false end false
                                                                   host ((:headers (req/ring-request)) "host")}}
  (let [battle (first (bf3.info/battle-info (bf3.db/get-battle gameid :start start :end end)))]
    (layout (when battle (list (into [:div.info]
                                     (conj (show-battle-info battle)
                                           [:a {:href (str "http://" host "/live/" (:server battle))}
                                            "go live"]))
                               (->> battle :live  (take 1)
                                    (map #(-> % bf3.info/parse-info show-live-info))))))))

(defn- get-battles []
  (->> (client/get "http://work.dalkvist.se:8081/gc/battles/") :body))

(def battles (mem/memo-ttl get-battles *short-cache-time*))

(defpage "/gc/battles" []
  (html5 (battles)))

(defpage "/gc/battles/" {:keys [host] :or {host ((:headers (req/ring-request)) "host")}}
  (layout [:style {:type "text/css"} (gaka/css [:.battles :float "left"
                                                [:span :margin "0 1px"]
                                                [:.user
                                                 [:.name
                                                  :width "210px"
                                                  :display "inline-block"]]] )]
          (into [:div#battles]
                (for [btls (partition-by :server (battle-info))]
                  (into [:ul.battles]
                        (for [battle  btls]
                          [:li.battle
                           (into [:div.info]
                                 (conj (show-battle-info battle)
                                       [:a {:href
                                            (str "http://" host
                                                 (if (= "localhost:8081" host) "/get-battle/" "/battle/")
                                                 (:gameId battle) "?start=" (-> battle :time :start)
                                                 "&end="  (-> battle :time :end))}
                                        "show score"]))
                           [:ul.users
                            (for [user (->> (:users battle) (sort-by :clanTag)
                                            (partition-by :clanTag)
                                            (mapcat #(sort-by :personaName %)))]
                              [:li.user [:span.name (str (when-not (empty? (:clanTag user))
                                                           (str "[" (:clanTag user) "]"))
                                                         (:personaName user))]
                               [:span.expansions (bl/get-expansion-img (:expansions user) true)]])]

                           ]))))))

(defpage "/live" []
  (response/redirect "/live/" ))

(defpage "/live/" {:keys [server]}
  (layout (->> (if server server "87989727-762a-420f-8aab-48987c6a4dca")
               bf3.bl/get-live-info bf3.info/parse-info show-live-info)))

(defpage "/live/:server" {:keys [server]}
  (layout (->> (if server server "87989727-762a-420f-8aab-48987c6a4dca")
               bf3.bl/get-live-info bf3.info/parse-info show-live-info)))

(defpage "/live/:server/.json" {:keys [server]}
  (res/json (->> server bf3.bl/get-live-info bf3.info/parse-info )))

(defpage  "/gc/update" [] (layout [ (count (bl/save-live-users))
                                    (count (ts/save-live-users))]))

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
   (vals (merge bl/gc-platoones  bl/gc-pride))
   [:h1 "Play with your fellow GC soldiers"]
   [:small "Shows players in the GC platoons, "
    (link-to "http://battlelog.battlefield.com/bf3/platoon/2832655391300768492/" "Platoon 1")
    " "
    (link-to "http://battlelog.battlefield.com/bf3/platoon/2832655391533545956/" "Platoon 2")
    " "
    (link-to "http://battlelog.battlefield.com/bf3/platoon/2832655391785121883/" "Platoon 3")
    ". "]
   [:small
    (link-to "http://www.global-conflict.org/viewtopic.php?f=3&t=16475" "How to sign up for the platoons")]
   [:small "Inspired by " (link-to "https://stalkdice.ep.io/" "stalk dice")]
    ))

(defpage "/stalking/gc/pride" []
  (stalking-layout
   (vals bl/gc-pride)
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
