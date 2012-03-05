(ns bf3.stats
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [bf3.bl :as bl])
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

(defn get-battleday
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

(defn- get-test-stats []
  (-> (client/get "http://bf3.herokuapp.com/gc/bl-stats.json" )
      :body
      (parse-string true)))


(def test-stats (mem/memo-ttl get-test-stats *cache-time*))


(defn- stat-interval [stat]
  (time/interval (time-format/parse (:first-seen stat))
                 (time-format/parse (:last-seen stat))))

(defn- attended-battleday [user-stats]
  (filter #(time/overlaps? ;; (get-battleday)
            (time/interval (time/date-time 2012 3 3 18)
                           (time/date-time 2012 3 4 0))
            (stat-interval %)) user-stats))

(defn- get-active-users [stats]
  (->> stats (map :user) distinct (pmap #(->> % bl/get-username ))
       (filter #(not-empty %))))

;;TODO take used servers as list, alt check if bl can give private flag
(defn- roster-last-battleday ([] (roster-last-battleday (test-stats)))
  ([stats] (->> stats
                (mapcat #(for [stat (last %)]
                           (assoc stat :user (first %))))
                attended-battleday
                (filter (fn [s] (some #(= % (:server s)) (vals bl/server-ids))))
                (sort-by :server)
                (partition-by :server)
                (map #(hash-map :server (:server (first %))
                                :users (->> %
                                            get-active-users
                                            (map s/lower-case)
                                            distinct
                                            sort))))))

(def battleday-roster (mem/memo-ttl roster-last-battleday *cache-time*))
