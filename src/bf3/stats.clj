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

(defn- test-users3 [] (-> (client/get "http://bf3.herokuapp.com/gc/bl-users.json" )
                        :body
                        (parse-string true)))

(defn- get-battleday
  "Return the a battledat interval, :prev for the latest one, and :next for the next one"
  ([] (get-battleday :prev))
  ([with] (->> (time/now) (time/day-of-week)
               ((fn [weekday] (if (= :next with)
                      (->> weekday (#(+ % 6)) (#(mod % 7)) (#(if (= % 0) 7 %)) time/days (time/plus  (time/now)))
                      (->> weekday (#(- 6 %)) Math/abs time/days (time/minus (time/now))))))
               (#(time/date-time (time/year %) (time/month %) (time/day %) 18 0 0 0))
               (#(time/interval % (time/plus % (time/hours 6) ))))))


(defn- parse-stats [rows]
  (let [users (atom {})
        stats (atom {})
        current-time (:time (last rows))]

    (doseq [server-rows (->> rows (sort-by :server) (partition-by :server))]
      (doseq [row (sort-by :time server-rows)]
        ;;logg all online users
        (doseq [user (:users row)]
          (if (contains? @users user)
            (swap! users assoc-in [user :last-seen]  (:time row))
            (swap! users assoc user {:first-seen (:time row) :server (:server row)})
            ))

        ;;update offline users
        (doseq [[user stat] @users]
          (when (not-any? #(= % user) (:users row))
            (swap! stats assoc user (conj (get @stats user []) stat))
            (swap! users dissoc user)))
        )

      ;;update users currently online
        (doseq [[user stat] @users]
          (swap! stats assoc user (conj (get @stats user [])
                                        (if (nil? (:last-seen stat))
                                          (assoc stat :last-seen current-time)
                                          stat)))
          (swap! users dissoc user))

      ;; (doseq [user @users]
      ;;   ;; (println "u:" user "\nid:" (key user) "\nstats:" (val user))
      ;;   (swap! intervals merge (get @intervals (key user) {})
      ;;          {(key user) (first (for [server (val user)]
      ;;                               (do ;; (println "s:" server "\nsid:" (key server) "\nss:" (val server))
      ;;                                 (if-not (:last-seen (val server))
      ;;                                   {(key server) (assoc (val server) :last-seen current-time)}
      ;;                                   (apply hash-map server)))))})
      ;;   (swap! users dissoc (key user)))
      )
    @stats))

(defn- get-live-stats
  "get user stats from the coll"
  ([coll]
     (sort-by first (parse-stats coll))))

(def get-stats (mem/memo-ttl get-live-stats *cache-time*))

(defn- test-stats []
  (-> (client/get "http://bf3.herokuapp.com/gc/bl-stats.json" )
      :body
      (parse-string true)))

(defn- get-active-users [stats]
  (->> stats (map first) distinct (pmap #(->> % bf3.bl/get-username ))
       (filter #(not-empty %))))
