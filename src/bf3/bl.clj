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

(defn- get-current-iso-8601-date
  "Returns current ISO 8601 compliant date."
  []
  (let [current-date-time (time/to-time-zone (time/now) (time/default-time-zone))]
    (time-format/unparse
     (time-format/with-zone (time-format/formatters :date-time-no-ms)
       (.getZone current-date-time))
     current-date-time)))

(def players-on-server-url "http://battlelog.battlefield.com/bf3/servers/getPlayersOnServer/")

(defn- get-players-on-server [server-id]
  (-> (client/get (str players-on-server-url server-id "/" ))
      :body
      (parse-string true)))

(def server-ids (hash-map
           :eu          "bc442e68-e072-4151-86da-cdba81c51cf5"
           :na-chicago  "14fb6dc4-1f57-4920-938b-73b0921b9303"
           :na-new-york "2da0845c-463d-4452-93ab-51294898474d"))

(def ^{:dynamic true} *cache-time* (* 2 60 1000))

(defn- get-live-users
  "get user from the battlelog server"
  ([] (get-live-users (->> server-ids vals first)))
  ([id]
     (hash-map :time (get-current-iso-8601-date)
               :users (->> (get-players-on-server id)
                           :players
                           (map #(-> % :persona :personaId)))
               :server id)))

(def get-users (mem/memo-ttl get-live-users *cache-time*))

(defn- get-origin-username [soldier-id]
  (->  (client/get (str "http://battlelog.battlefield.com/bf3/overviewPopulateStats/" soldier-id "/a/1/"))
       :body (parse-string true)
       :data :user :username))

(def get-username (mem/memo-ttl get-origin-username *cache-time*))

(defn save-live-users []
  (doseq [[server id] server-ids]
    (save-bl-user! (get-users))))

(defn -main [& m]
 (save-live-users))
