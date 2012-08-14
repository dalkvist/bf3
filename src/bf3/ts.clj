(ns bf3.ts
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

(def ts-url "http://www.tsviewer.com/index.php?page=ts_viewer&ID=995941")

(def ^{:dynamic true} *cache-time* (* 2 60 1000))

(defn- get-live-users
  "get user from the ts info url"
  ([] (get-live-users ts-url))
  ([url]
     (->> (-> url URL. html-resource
              (select [:.ts3v_user]))
          (map :content)
          (apply concat))))

(def get-users (mem/memo-ttl get-live-users *cache-time*))

(defn save-live-users []
  (save-ts-user! (hash-map :time (get-current-iso-8601-date) :users (get-users))))

(defn -main [& m]
 (save-live-users))
