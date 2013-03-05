(ns bf3.info
  (:require [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use [bf3.db]
        [bf3.bl :only [get-username get-user]])
  (:import java.net.URL))

(def ^{:dynamic true} *cache-time* (* 2 60 1000))

(defn- get-battle-info [battle]
  (merge (select-keys (first battle) [:server])
               (:info   (first battle))
               (hash-map :users (->> (mapcat :users battle)
                                     distinct
                                     (map #(if (string? %)
                                             (->> % get-username get-user)
                                             (if (vector? %)
                                               (flatten %)
                                               [%])))
                                     (apply concat)
                                     (sort-by :userId)
                                     (partition-by :userId)
                                     (map first))
                         :start (->> (sort-by :time battle)
                                     first :time)
                         :end (->> (sort-by :time battle)
                                     last :time))))

(defn- get-battle-infos []
  (->> (get-bl-users)
       (filter #(not-empty (:users %)))
       (sort-by :server)
       (partition-by :server)
       (mapcat (fn [x] (partition-by #(select-keys % [:info :gameId]) x)))
       (pmap get-battle-info)
       ))

(def battle-info (mem/memo-ttl get-battle-infos *cache-time*))
