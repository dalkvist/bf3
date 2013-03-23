(ns bf3.bl
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use [cheshire.core]
        [net.cgrand.enlive-html]
        [bf3.db]
        [bf3.info :only [parse-info]])
  (:import [java.net URI DatagramPacket DatagramSocket InetAddress MulticastSocket]))

(declare get-user get-live-info)

(def ^{:dynamic true} *cache-time* (* 2 60 1000))

(def ^{:dynamic true} *long-cache-time* (* 24 60 60 1000))

(def ^{:dynamic true} *short-cache-time* (* 1 60 1000))

(def ^{:dynamic true} *tiny-cache-time* (* 10 1000))

(def maps {
           "MP_001" "Grand Bazaar"
           "MP_003" "Teheran Highway"
           "MP_007" "Caspian Border"
           "MP_011" "Seine Crossing"
           "MP_012" "Operation Firestorm"
           "MP_013" "Damavand Peak"
           "MP_017" "Noshahar Canals"
           "MP_018" "Kharg Island"
           "MP_Subway" "Operation Metro"
           "XP1_001" "Strike At Karkand"
           "XP1_002" "Gulf of Oman"
           "XP1_003" "Sharqi Peninsula"
           "XP1_004" "Wake Island"
           "maps-XP2_Factory" "Scrapmetal"
           "XP2_Office" "Operation 925"
           "XP2_Palace" "Donya Fortress"
           "XP2_Skybar" "Ziba Tower"
           "XP3_Alborz" "Alborz Mountains"
           "XP3_Desert" "Bandar Desert"
           "XP3_Shield" "Armored Shield"
           "XP3_Valley" "Death Valley"
           "XP4_FD" "Markaz Monolith"
           "XP4_Parl" "Azadi Palace"
           "XP4_Quake" "Epicenter"
           "XP4_Rubble" "Talah Market"
           "XP5_001" "Operation Riverside"
           "XP5_002" "Nebandan Flats"
           "XP5_003" "Kiasar Railroad"
           "XP5_004" "Sabalan Pipeline"
           })

(def mapModes {
               1 "Conquest"
               128 "Conquest Assault Large"
               256 "Conquest Assault Small"
               64 "Conquest Large"
               1024 "Domination"
               512 "Gun Master"
               16 "Onslaught"
               2 "Rush"
               8 "Squad DM"
               4 "Squad Rush"
               32 "Team DM"
               2048 "Team DM CQ"
               131072 "Tank Superiority"
               4194304 "Scavenger"
               524288 "Capture the Flag"
               8388608 "Air Superiority"
               })

(def serverPrests {1  "Normal"
                   2  "Hardcore"
                   4  "Infantry Only"
                   8  "Custom"
               })

(def server-settings {:vnta "Show Enemy Name Tags",
                      :vtkc "Team Kills Before Kick",
                      :vhud "Show Hud",
                      :vaba "Team Balance",
                      :vshe "Health",
                      :vnit "Idle Time Before Kick",
                      :vvsa "Vehicles",
                      :vbdm "Bullet Damage",
                      :osls "Only Squad Leader Spawn",
                      :vmsp "Use Minimap Spotting",
                      :vpmd "Man Down Time",
                      :vmin "Show Minimap",
                      :vtkk "Kicks Before Ban",
                      :vkca "Kill Cam",
                      :vrhe "Regenerative Health",
                      :v3ca "3p Vehicle Cam",
                      :vprt "Respawn Time",
                      :v3sp "Use 3d Spotting",
                      :vffi "Friendly Fire"})

(def expansions {:1024  "Premium",
                 :512   "Back to Karkand",
                 :2048  "Close Quarters",
                 :4096  "Armored Kill",
                 :8192  "Aftermath"
                 :16384 "End Game"})

(defn- get-current-iso-8601-date
  "Returns current ISO 8601 compliant date."
  []
  (let [current-date-time (time/to-time-zone (time/now) (time/default-time-zone))]
    (time-format/unparse
     (time-format/with-zone (time-format/formatters :date-time-no-ms)
       (.getZone current-date-time))
     current-date-time)))

(def players-on-server-url "http://battlelog.battlefield.com/bf3/servers/getPlayersOnServer/")

(defn get-server-url [id]
  (str "http://battlelog.battlefield.com/bf3/servers/show/" id "/a/"))

(defn- get-server-info [id]
  (-> (client/get (get-server-url id)
                  {:headers {"X-AjaxNavigation" "1"}})
      :body (parse-string true) :context))

(def server-info (mem/memo-ttl get-server-info *tiny-cache-time*))

(def server-ids {:na-new-york "2da0845c-463d-4452-93ab-51294898474d"
                 :eu "bc442e68-e072-4151-86da-cdba81c51cf5"})

(def gc-platoones (hash-map :p1 "2832655391300768492" :p2 "2832655391533545956" :p3 "2832655391785121883"))

(def dice-platoones (hash-map :p1 "2832655391300702826" :p2 "2832655391301004520"))

(def gc-pride (hash-map :p1 "2832655391698814149"))

(defn- user-loadouts [user]
  (try
    (-> (client/get (str "http://battlelog.battlefield.com/bf3/soldier/loadoutPopulateStats/"
                         (:personaName user) "/" (:personaId user) "/" (:userId user)  "/1/0/")
                    {:headers {"X-Requested-With" "XMLHttpRequest"}})
        :body (parse-string true)
        :data
        (#(assoc user
            :loadouts (:currentLoadouts %)
            :clanTag (:personaClanTag %))))
    (catch Exception ex
      user)))

(def get-user-loadouts (mem/memo-ttl user-loadouts *cache-time*))

(defn- get-live-users
  "get user from the battlelog server"
  ([] (get-live-users (->> server-ids vals first)))
  ([id]
     (let [info (server-info id)
           live (get-live-info (get-in info [:server :ip]) (get-in info [:server :port])
                                         (get-in info [:server :gameId]))]
       (merge {:time (get-current-iso-8601-date) :server id :live live}
              (hash-map :vehicles (= 1 (get-in info [ :server :settings :vvsa])))
              (select-keys (:server info) [:hasPassword :gameId :map :mapMode :mapVariant :name :matchId])
              (parse-info live)))))

(def get-users (mem/memo-ttl get-live-users *tiny-cache-time*))


(defn- get-test-users [] (-> (client/get "http://bf3.herokuapp.com/gc/bl-users.json" )
                        :body
                        (parse-string true)))

(def test-users (mem/memo-ttl get-test-users *cache-time*))

(defn- get-origin-info [soldier-id]
  (try
       (-> (client/get (str "http://battlelog.battlefield.com/bf3/overviewPopulateStats/"
                            soldier-id "/a/1/"))
           :body (parse-string true)
           :data :user (select-keys [:username :userId]))
       (catch Exception ex)))

(def origin-info (mem/memo-ttl get-origin-info *cache-time*))

(defn get-username [soldier-id]
  (:username (origin-info soldier-id)))

(defn get-userid [soldier-id]
  (:userId (origin-info soldier-id)))

(defn- user-by-name [name]
  (-> (client/get (str "http://battlelog.battlefield.com/bf3/user/" name "/")
                  {:headers {"X-AjaxNavigation" "1"}})
      :body (parse-string true) :context :soldiersBox))

(defn- user-by-id [id]
  (-> (client/get (str "http://battlelog.battlefield.com/bf3/user/overviewBoxStats/" id "/")
                  {:headers {"X-AjaxNavigation" "1"}})
      :body (parse-string true) :data :soldiersBox))

(defn- get-user-info [user]
  (->> (if (try (every? #(Integer/parseInt (str %)) user)
                (catch Exception ex false))
         (user-by-id user)
         (user-by-name user))
       (filter #(= "cem_ea_id" (s/lower-case (get-in % [:persona :namespace]))))
       (map :persona)
       (map #(select-keys % [:userId :personaId :clanTag :personaName]))))

(def get-user (mem/memo-ttl get-user-info *cache-time*))

(defn- user-expansions [name]
  (filter #(not (nil? (expansions %)))
          (map #(key %) (-> (client/get (str "http://battlelog.battlefield.com/bf3/user/" name "/")
                                        {:headers {"X-AjaxNavigation" "1"}})
                            :body (parse-string true)
                            :context

                            ))))

(def get-user-expansions (mem/memo-ttl user-expansions *long-cache-time*))

(defn get-expansion-img [ids only-premium?]
  (map #(vector :img
                 {:src (str "http://battlelog-cdn.battlefield.com/public/common/tags/expansion/"
                            (->> % str (drop 1) (reduce str))
                            ".gif")
                  :alt (expansions %)})
       (let [p (filter #(= % :1024) ids)]
         (if (and only-premium? (not-empty p))
           p
           ids))))

(defn save-live-users
  ([] (save-live-users (time/now)))
  ([start]
     (when (> 60 (time/in-secs (time/interval start (time/now))))
       (doseq [[server id] server-ids]
         (let [info (get-users id)]
           (when (not-empty info) (save-bl-user! info))))
       (when (> 45 (time/in-secs (time/interval start (time/now))))
         (future (Thread/sleep 10000)
                 (save-live-users start))))))

(defn- get-platoon-info [id]
  (-> (client/get (str "http://battlelog.battlefield.com/bf3/platoon/" id "/listmembers/")
                  {:headers {"X-AjaxNavigation" "1"}})
      :body (parse-string true) :context))

(def platoon-info (mem/memo-ttl get-platoon-info *short-cache-time*))

(defn get-playing-users [ids]
  (let [users (->> ids (mapcat #(->> (platoon-info %)
                                     :listMembers
                                     (filter (fn [u] (not (nil? (->> u :user :presence :serverGuid)))))
                                     )))
        servers (->> users (map #(->> % :user :presence :serverGuid))
                     distinct
                     (map #(server-info %)))]
    (->> servers (map (fn [s]
                        (hash-map (->> s :server :guid)
                                  (hash-map :server s
                                            :users
                                            (->> users
                                                 (filter #( = (get-in s [:server :guid])
                                                             (get-in % [:user :presence :serverGuid]))))))))
         (apply merge))))


(defn -main [& m]
 (save-live-users))

(def ^{:Dynamic true} default-dest-port 55000)

(defn live-info
  ([server] (let [serverinfo (if (string? server) (->> server (bf3.bl/server-info) :server)
                                 (if (map? server) server))]
              (when serverinfo (live-info (:ip serverinfo) (:port serverinfo) (:gameId serverinfo)))))
  ([server-ip server-port game-id]
     (live-info server-ip server-port game-id default-dest-port))
  ([server-ip server-port game-id dest-port]
     (try
       (let [socket (MulticastSocket. dest-port)
             info (live-info server-ip server-port game-id dest-port socket)]
         (.close socket)
         info)
       (catch Exception ex
         (when (and (not (nil? (re-find #"Address already in use" (.getMessage ex))))
                    (< (- dest-port default-dest-port) 50))
           (live-info server-ip server-port game-id (int (+ (rand 25) dest-port)))))))
  ([server-ip server-port game-id dest-port socket]
     (let
         [ubyte #(if (>= % 128)
                   (byte (- % 256))
                   (byte %))
          padding "ffffffff51505f000000000"
          gameid (Integer/toHexString (if (string? game-id) (Integer/parseInt game-id) game-id))
          msg (->> (str padding gameid)
                   (partition-all 2) (map #(reduce str %))
                   (map #(Integer/parseInt (str %) 16))
                   (map ubyte)
                   (byte-array))
          ip (InetAddress/getByName server-ip)
          ping (DatagramPacket. msg (count msg) ip server-port)
          pong (DatagramPacket. (byte-array 16384) 16384)
          ]
       (.setTimeToLive socket 125)
       (.setSoTimeout socket 1000)
       (.send socket ping)
       (.disconnect socket)
       (.receive socket pong)
       (.disconnect socket)
       (let [extrapadding (->> (.getData pong)
                               (map str)
                               (take-while #(not (= "0" %)))
                               (map #(Integer/parseInt %))
                               (map byte)
                               (byte-array))
             request-msg (byte-array (concat msg extrapadding))
             request (DatagramPacket. request-msg (count request-msg) ip server-port)]
         (.send socket request))
       (.disconnect socket)
       (.receive socket pong)
       (.disconnect socket)
       (.close socket)
       (->> (.getData pong)
            (take (.getLength pong))
            (map identity)))))

(def get-live-info (mem/memo-ttl live-info *tiny-cache-time*))
