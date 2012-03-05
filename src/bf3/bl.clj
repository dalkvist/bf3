(ns bf3.bl
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use [cheshire.core]
        [net.cgrand.enlive-html]
        [bf3.db])
  (:import java.net.URL))


(def ^{:dynamic true} *cache-time* (* 2 60 1000))

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
           })

(def mapModes {1  "Conquest"
               2  "Rush"
               4  "Squad Rush"
               8  "Squad DM"
               32 "Team DM"
               64 "Conquest Large"
           })

(defn- get-current-iso-8601-date
  "Returns current ISO 8601 compliant date."
  []
  (let [current-date-time (time/to-time-zone (time/now) (time/default-time-zone))]
    (time-format/unparse
     (time-format/with-zone (time-format/formatters :date-time-no-ms)
       (.getZone current-date-time))
     current-date-time)))

(def players-on-server-url "http://battlelog.battlefield.com/bf3/servers/getPlayersOnServer/")

(defn- get-server-info [id]
  (-> (client/get (str "http://battlelog.battlefield.com/bf3/servers/show/" id "/a/")
                  {:headers {"X-AjaxNavigation" "1"}})
      :body (parse-string true) :context))

(def server-info (mem/memo-ttl get-server-info *cache-time*))

(def server-ids (hash-map
           :eu          "bc442e68-e072-4151-86da-cdba81c51cf5"
           :na-chicago  "14fb6dc4-1f57-4920-938b-73b0921b9303"
           :na-new-york "2da0845c-463d-4452-93ab-51294898474d"))

(defn- get-live-users
  "get user from the battlelog server"
  ([] (get-live-users (->> server-ids vals first)))
  ([id]
     (hash-map :time (get-current-iso-8601-date)
               :users (->>  id server-info :players
                            (map #(-> % :persona :personaId)))
               :server id
               :info (-> id (server-info) :server
                         (select-keys  [:hasPassword :gameId :map :mapMode])))))

(def get-users (mem/memo-ttl get-live-users *cache-time*))


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

(defn save-live-users []
  (doseq [[server id] server-ids]
    (save-bl-user! (get-users id))))

(defn -main [& m]
 (save-live-users))
