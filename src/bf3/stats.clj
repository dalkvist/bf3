(ns bf3.stats
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

(def ^{:dynamic true} *debug* false)

(defn debug [& messages]
  (when *debug*
    (println messages)))

(defn- test-users []
  (parse-string "[{\"time\":\"2012-03-01T19:07:19Z\",\"users\":[\"[HA6]Run3\",\"[K1TO]me8myself\",\"[K1VD] Robawillis\",\"[K11L]Calloutman\",\"[HA6]m4rvk\",\"[HI9]FluidicTapestry\",\"[K11B]cairdazar\",\"[K22B]SirSidd\",\"[HA9]Liljedahls\",\"[HI6]Mordred\",\"[K1MD].Sup\",\"[K22T]Testicular Power\"]}, {\"time\":\"2012-03-01T19:13:22Z\",\"users\":[\"[HA6]Run3\",\"[K22T]Testicular Power\",\"[K1TO]me8myself\",\"[K1VD] Robawillis\",\"[K11L]Calloutman\",\"[HA6]m4rvk\",\"[HI9]FluidicTapestry\",\"[K11B]cairdazar\",\"[K22B]SirSidd\",\"[HI5]Ash\",\"[HI6]Mordred\",\"[K1MD].Sup\"]}, {\"time\":\"2012-03-01T20:41:06Z\",\"users\":[\"[HI9]FluidicTapestry\",\"[HA6]Run3\",\"[HI6]Mordred\",\"[HA8]KoffeinFlummi\",\"[HI8]fullmetaljacket9\",\"[K1TO]me8myself\",\"[HH1]Hitman\",\"[HA6]m4rvk\",\"[K22B]SirSidd\",\"[K1VD] Robawillis\",\"[K2AD]a432\",\"[K11L]Calloutman\",\"[K12L]FlashofSilver\",\"[K22L]PKROCKY\",\"[K22L] Rolten\",\"[K22L]arcturus-dsf\",\"[K22T]FlaminWalrus\",\"DrunkenMc\",\"[KCEO] Shrapnel\",\"[K12B]Zimmin\",\"[K11B] Rev0lver3\",\"[K22T] dan1mall\",\"[K22T]LongBowNL\"]}]" true))

(defn- test-users2 [] (-> (client/get "http://bf3.herokuapp.com/gc/ts-users.json" )
                        :body
                        (parse-string true)))

(defn- parse-stats [rows]
  (let [users (atom {})
        intervals (atom {})]
    (doseq [row (sort-by :time rows)]
      (doseq [user (:users row)]
        (when (= "2012-03-01T22:36:06Z" (:time row)
                 ;; "[KCEO] Shrapnel"
                 ;; "[K11B]cairdazar"
                 )
          (debug user " time: " (:time row) "  contains?" (contains? @users user)))
        (if (contains? @users user)
          (swap! users assoc user (assoc (get @users user) :last-seen (:time row)))
          (swap! users assoc user (hash-map :first-seen (:time row)))))
      (doseq [user (keys @users)]
        (when (= "2012-03-01T22:41:06Z" (:time row))
          (debug user " time: " (:time row) "  contains?" (contains? @users user)
                   " not-any? " (not-any? #(= % user) (:users row))))
        (when (not-any? #(= % user) (:users row))
          (swap! intervals assoc user (conj (get @intervals user []) (get @users user) ))
          (swap! users dissoc user))))
    (doseq [user (keys @users)]
      (when-not (:last-seen user)
        (swap! intervals assoc user
               (conj (get @intervals user [])
                     (assoc (get @users user)
                       :last-seen (:time (last rows)))))))
    @intervals))

(defn- get-live-stats
  "get user stats from the coll"
  ([coll]
     (sort-by first (parse-stats coll))))

(def get-stats (mem/memo-ttl get-live-stats *cache-time*))
