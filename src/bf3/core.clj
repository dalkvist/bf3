(ns bf3.core
  (:use [ring.adapter.jetty]
        [compojure.core]
        noir.core
        hiccup.core
        hiccup.page-helpers
        [bf3.db :only [get-ts-users get-bl-users get-battle]]
        [bf3.live :only [parse-info]]
        [bf3.info :only[battle-info merge-infos]]
        [cheshire.core :only [encode generate-string]])
  (:require (compojure [route :as route])
            (ring.util [response :as response])
            (ring.middleware [multipart-params :as mp])
            [noir.response :as res]
            [noir.request :as req]
            [noir.server :as server]
            [clojure.string :as s]
            [gaka [core :as gaka]]
            [bf3.bl :as bl]
            [bf3.redis :as redis]
            [clj-http.client :as client]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [clj-time.coerce :as time-cc]))

(def ^{:dynamic true} *tiny-cache-time* (* 9 1000))
(def ^{:dynamic true} *short-cache-time* (* 59 1000))
(def ^{:dynamic true} *extra-long-cache-time* (* 12 60 60 1000))


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
    (include-js "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js")
    ]
    [:body
     [:div#wrapper
      content
      ]]))

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

(defpartial battle-layout [& content]
  (layout  [:style {:type "text/css"} (gaka/css [:div.clear :clear "both"]
                                                [:.left :float "left"]
                                                [:.right :float "right"]
                                                [:.info :max-width "980px"]
                                                [:.battles :float "left"
                                                  [:li
                                                   :background-color "#ccc"
                                                   :border "1px solid #666"
                                                   :border-radius "20px"
                                                   :padding "10px"
                                                   :margin "5px 0px"
                                                   :list-style-type "none"
                                                   [:.info
                                                    [:h3
                                                     :font-weight "normal"
                                                     :margin "0 0 5px 0"]
                                                    [:.right :margin "0 3px"
                                                     [:span :margin "2px"]]
                                                    [:div [:> [:span :display "block"]]]]
                                                   [:span :margin "0 1px"]
                                                   [:a.toggle.score :display "none"]
                                                   [:.livecontainer :display "none"]
                                                   [:ul.users :display :none
                                                    [:.user
                                                     [:.name
                                                      :width "210px"
                                                      :display "inline-block"]]]]]
                                                [:.expansions :display "none"]
                                                [:.duration :display "block"]
                                                [:a.toggle.users :display "none"]
                                                [:.livecontainer
                                                 :background-color "#fff"
                                                 :padding "10px"
                                                 :margin "10px 0"
                                                 :border-radius "20px"
                                                 [:.team :float "left"
                                                  :margin "10px"
                                                  :width "475px"
                                                  [:.score :float "right"
                                                   :text-align "center"
                                                   :width "100%"
                                                   [:.max :float "left"
                                                    :font-size "12px"]
                                                   [:.current :float "right"
                                                    :font-weight "bold"
                                                    :font-size "18px"
                                                    :margin-top "-2px"]
                                                   [:.bar :display "inline-block"
                                                    :width "85%"
                                                    :height "6px"
                                                    :padding "1px"
                                                    :margin "3px 5px"
                                                    :border "1px solid #cccccc"
                                                    [:span :float "right"
                                                     :height "100%"
                                                     :background-color "#000000"
                                                     :margin "0px"]
                                                    [:.bleed
                                                     :background-color "rgba(200, 0, 0, 0.55)"]
                                                    [:.death
                                                     :background-color "rgba(255, 0, 0, 0.33)"]]]
                                                  [:.stats :clear :both
                                                   :text-align "center"
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
                                                    [:* :float "left"]]]
                                                  [:tr.inactive :color "#CCCCCC"]]
                                                 [:#t2 [:.score :float "left"
                                                        [:.max :float "right"]
                                                        [:.bar [:span :float "left"]]
                                                        [:.current :float "left"]]]] )]
           content))

(def rank-url "http://battlelog-cdn.battlefield.com/cdnprefix/14d47a6313f98/public/profile/bf3/stats/ranks/tiny/")

(def map-preview-url "http://battlelog-cdn.battlefield.com/cdnprefix/9aa162d40ad4/public/base/bf3/map_images/146x79/")

(defn get-show-live-info [liveinfo]
  [:div.livecontainer
   (for [team (partition-by :team (sort-by :team (:users liveinfo)))]
     (let [t (if (keyword? (:team (first team)))
                                (:team (first team))
                                (keyword (str (:team (first team)))))]
       [:div.team {:id (str "t" (name (:team (first team))))}
        ((fn [score]
           [:div.score
            [:span.max (:max score)]
            (into [:span.bar]
                  (letfn [(get-width [k] (if (or (= (:max score) 0) (= (get score k 0) 0)) 0
                                             (- (float (* 100 (/ (k score) (:max score)))) 1)))]
                    (concat (list [:span {:style (str "width:" (get-width :current) "%;")}])
                            (if (re-find #"conquest" (s/lower-case (str (:gameMode liveinfo))))
                              (list [:span.bleed {:style (str "width:"  (get-width :bleed) "%;")}]
                                    [:span.death {:style (str "width:" (get-width :deaths) "%;")}])))))
            [:span.current (:current score)]])
         (get (:stats liveinfo) t {}))
        (into [:div.stats] (concat [[:h4 (char 0x03a3)]]
                                   (map #(vector :span {:class  (name (first %))} [:h5 (name (first %)) ":"]
                                                 [:span (str (second %))])
                                        (merge (dissoc (get (:stats liveinfo) t)
                                                       :max :current)
                                               {:players (count team)}))))
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
                          (into [:tr {:class (if (false? (:online (first users))) "inactive" "")}]
                                (->> [ pos (if (< 0 (:squad (first users))) (char (+ 64 (:squad (first users)))) "")
                                      (list (#(when (< % 146) (vector :img {:src (str rank-url (if (< 45 %)  (str "ss" (- % 45))
                                                                                                   (str "r" %))  ".png")
                                                                             :height "23px" :width " 29px"}))
                                             (:rank (first users)))
                                            [:span.tags (let [tags (:clanTags (first users))]
                                                          (when (not-empty tags) (str "[" tags "]")))]
                                            [:span.name (:personaName (first users))])
                                      (:kills (first users))
                                      (:deaths (first users)) (:score (first users))]
                                     (map #(vector :td  (if (coll? %) % (str %)))))))))))]]))
   [:div.clear]])


(def show-live-info-long (mem/memo-ttl get-show-live-info *extra-long-cache-time*))
(def show-live-info-short (mem/memo-ttl get-show-live-info *tiny-cache-time*))

(defn show-live-info [battle]
  (if (< 2 (time/mins-ago (time-format/parse (:end (:time battle)))))
    (show-live-info-long battle)
    (show-live-info-short battle)))

(defn get-battle-info [battle]
  [[:div.left
    [:img {:src (str map-preview-url (s/lower-case (get battle :map "")) ".jpg")}]]
   [:div.right
    [:h3 (if-let [name (->> battle :server :name)]
            name
            (->> (:server battle) bl/server-info :server :name))]
    [:div.map.left
     [:span.name (bl/maps (:map battle))]
     [:span.mode (bl/mapModes (:mapMode battle))]
     [:span.variant (cond (= 1 (:mapVariant battle)) "Tiny (16 player)"
                          true "")]]
    [:div.time.right [:span.start [:span "start: "]
                (get-time-string (->> battle :time :start)) " utc"]
     [:span.end [:span "end: "] (get-time-string (->> battle :time :end)) " utc"]
     ;;TODO add relative to SBT
     [:span.duration [:span "duration: "]
      (->> (time/interval (clj-time.format/parse (->> battle :time :start))
                          (clj-time.format/parse (->> battle :time :end)))
           get-interval-string)]]]
   [:div.clear]])

(def show-battle-info-long (mem/memo-ttl get-battle-info *extra-long-cache-time*))
(def show-battle-info-short (mem/memo-ttl get-battle-info *short-cache-time*))

(defn show-battle-info [battle]
  (if (< 2 (time/mins-ago (time-format/parse (:end (:time battle)))))
    (show-battle-info-long battle)
    (show-battle-info-short battle)))

(defpage  "/" [] (layout "BF3 Stuff"))
(defpage  "/favicon.ico" [] "")

(defpage "/gc" []
  (response/redirect "/gc/"))
(defpage  "/gc/" [] (layout "GC stuff"))

(defn- get-battle-page [gameid & {:keys [start end] :or {start false end false}}]
  (let [id (str "gc-battle" gameid start end)
        data (redis/get-data id)
        save (fn [] (redis/set-data
                    id
                    {:time (+ *short-cache-time* (clj-time.coerce/to-long (clj-time.core/now)))
                     :data (->> (client/get (str "http://work.dalkvist.se:8081/get-battle/" gameid
                                                 "?" (encode-params
                                                      (merge {:host ((:headers (req/ring-request)) "host")}
                                                             (if start {:start start} {})
                                                             (if end   {:end end} {}))))) :body)}))]
    (if (or (not data) (nil? (:data data)) (empty? (:data data)))
      (do (save)
          (:data (redis/get-data id)))
      (do (when (and (number? (:time data))
                     (> (clj-time.coerce/to-long (clj-time.core/now)) (:time data)))
            (future (save)))
          (:data data)))))

(def battle (mem/memo-ttl get-battle-page *short-cache-time*))

(defpage "/battle/:gameid" {:keys [gameid start end]}
  (html5 (battle gameid :start start :end end)))


(defpage "/get-battle/:gameid" {:keys [gameid start end host] :or {start false end false
                                                                   host ((:headers (req/ring-request)) "host")}}
  (let [logs (bf3.db/get-battle  gameid :start start :end end)
        battle (-> (bf3.info/battle-info :logs logs) first)]
    (battle-layout (when battle (list [:style {:type "text/css"} (gaka/css [:div.livecontainer :display "block"])]
                                      (into [:div.info]
                                            (conj (show-battle-info battle)
                                                  [:a {:href (str "http://" host "/live/" (:server battle))}
                                                   "go live"]))
                                      (show-live-info (dissoc battle :live)))))))
(defn- get-battles [weeks]
  (let [data (redis/get-data (str "gc-battles/w" weeks))]
    (when (or (not data) (and (number? (:time data))
                              (> (clj-time.coerce/to-long (clj-time.core/now)) (:time data))))
      (future (redis/set-data
               (str "gc-battles/w" weeks)
               {:time (+ *short-cache-time* (clj-time.coerce/to-long (clj-time.core/now)))
                :data (->> (client/get (str "http://work.dalkvist.se:8081/gc/real-battles/" weeks
                                      "?" (encode-params {:host (get (:headers (req/ring-request)) "host" "")})))
                     :body)})))
    (:data data)))

(def battles (mem/memo-ttl get-battles *short-cache-time*))

(defpage "/gc/battles" {:keys [weeks] :or {weeks 0}}

  (html5 (battles weeks)))

(defpage "/gc/battles/:weeks" {:keys [weeks] :or {weeks 0}}

  (html5 (battles weeks)))

(defpage "/gc/real-battles/:weeks" {:keys [testservers host server weeks] :or {testservers false
                                                                   server false
                                                                   host ((:headers (req/ring-request)) "host")
                                                                   weeks 0}}
  (battle-layout
   (javascript-tag (str "$(document).ready(function(){"
                        "$('a.toggle.users').click(function(){"
                        "$(this).closest('li').find('ul.users').toggle(); return false});"
                        "$('a.livescore').click(function(){"
                        "var onlink = $(this); var offlink = $(this).parent().find('a.toggle.score');"
                        " $.get($(this).attr('href'), function (response) {"
                        "$(onlink).closest('li').find('.livecontainer')"
                        ".replaceWith($('.livecontainer', response));"
                        "$(onlink).closest('li').find('.livecontainer').toggle('true');"
                        "$(onlink).toggle('false');$(offlink).toggle('true');"
                        "});"
                        "return false;});"
                        "$('a.toggle.score').click(function(){"
                        "$(this).toggle(); $(this).parent().find('a.livescore').toggle();"
                        "$(this).closest('li').find('.livecontainer').toggle('false');"
                        "return false;});"
                        "});"))
   (into [:div#battles  [:p "To help with slow the loading, battles are now partitioned by weeks. "
                         "Use gc/battles/\"nr of weeks ago\" for previous weeks, gc/battles/1 gives last week etc"]]
         (for [btls (battle-info :weeks (try (Integer/parseInt weeks) (catch Exception ex 0)))]
           (into [:ul.battles]
                 (for [battle  btls]
                   (when (and (or (false? server) (= server (:server battle)))
                              (or (true? testservers)
                                  (some #(= % (:server battle)) (vals bf3.bl/server-ids)))
                              (< 15 (count (:users battle)))
                              (< (* 5 60)
                                 (time/in-secs (time/interval (clj-time.format/parse (->> battle :time :start))
                                                              (clj-time.format/parse (->> battle :time :end))))))
                     [:li.battle
                      (into [:div.info]
                            (concat ( show-battle-info battle)
                                    (list [:a {:class "livescore" :href
                                               (str "http://" host
                                                    (if (or(= "work.dalkvist.se:8081" host)
                                                           (= "localhost:8081" host)) "/get-battle/" "/battle/")
                                                    (:gameId battle) "?"
                                                    (encode-params {:start (-> battle :time :start)
                                                                    :end  (-> battle :time :end)}))}
                                           "show score"]
                                          [:a.toggle.score {:href ""} "hide score"])))
                      [:div.livecontainer]
                      (comment [:a.toggle.users {:href ""} "toggle users"]
                               [:ul.users
                                (for [user (->> (:users battle) (sort-by :clanTags)
                                                (partition-by :clanTags)
                                                (mapcat #(sort-by :personaName %)))]
                                  [:li.user [:span.name (str (when-not (empty? (:clanTags user))
                                                               (str "[" (:clanTags user) "]"))
                                                             (:personaName user))]
                                   [:span.expansions (bl/get-expansion-img (:expansions user) true)]])])

                      ])))))))

(defn- get-live [server]
  (let [id (str "live" server)
        data (redis/get-data id)
        save #(redis/set-data
               id
               {:time (+ *tiny-cache-time* (clj-time.coerce/to-long (clj-time.core/now)))
                :data (->> (client/get (str "http://work.dalkvist.se:8081/live/" server))
                           :body)})]

    (if (or (not data) (nil? (:data data)) (empty? (:data data)))
      (do (save)
          (:data (redis/get-data id)))
      (do (when (and (number? (:time data))
                     (> (clj-time.coerce/to-long (clj-time.core/now)) (:time data)))
            (future (save)))
          (:data data)))))

(def live (mem/memo-ttl get-live 2000))

(defpage "/live" {:keys [server] :or {server "eu"}}
  (html5 (live server) ))

(def live-js (javascript-tag (str "var runUpdate = true;"
                                  "var update = function(){$.get(window.location.href,"
                                  "function(res){if($('.livecontainer',res).children().length > 1){$('.livecontainer').replaceWith($('.livecontainer', res).first());}});"
                                  "if(runUpdate){ window.setTimeout(update, 10 * 1000);};};"
                                  "update();")))

(defpage "/live/" {:keys [server]}
  (battle-layout (list live-js
                       (->> (if server (condp = server
                                         "eu" :eu
                                         "na" :na-new-york
                                         server)
                                "87989727-762a-420f-8aab-48987c6a4dca")
                            bf3.bl/get-live-info parse-info show-live-info))))

(defpage "/live/:server" {:keys [server]}
  (battle-layout (list live-js
                       (->> (if server (condp = server
                                         "eu" :eu
                                         "na" :na-new-york
                                         server)
                                "87989727-762a-420f-8aab-48987c6a4dca")
                            bf3.bl/get-live-info parse-info show-live-info))))

(defpage "/live/:server/.json" {:keys [server]}
  (res/json (->> server bf3.bl/get-live-info parse-info )))

(defpage  "/gc/update" []
  (layout (do (bl/save-live-users)
              "loggin attendance on GC server")))

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

(defn cache-battles
  [handler]
  (fn [request]
    (let [resp (handler request)]
      (if (= (:uri request) "/gc/battles")
        (assoc-in resp [:headers "Cache-Control"] "public; max-age=60")
        (if (re-find #"^/live/" (:uri request))
          (assoc-in resp [:headers "Cache-Control"] "public; max-age=10")
          resp)))))

(defonce server (atom nil))

(defn -main [& m]
  (server/add-middleware cache-battles)
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8081"))]
    (reset! server (server/start port))))

(defn export-stats
  (doseq [weeksago (range 3 10)]
    (spit (str "/tmp/GC-week-" (- 21 weeksago) ".js")
          (->> (battle-info :weeks weeksago)
               (map  (fn [l] (->> l
                                 (filter (fn [battle]
                                           (and (some (fn [ser] (= ser (:server battle))) (vals bf3.bl/server-ids))
                                                (< 15 (count (:users battle)))
                                                (< (* 5 60)
                                                   (time/in-secs (time/interval (clj-time.format/parse (->> battle :time :start))
                                                                                (clj-time.format/parse (->> battle :time :end))))))))
                                 (map (fn [battle] (dissoc battle :live))))))
               (filter not-empty)
               cheshire.core/generate-string))))
