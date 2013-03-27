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

(defn- inc-date [dt]
  (time/plus dt (time/days 1)))

(defn- dec-date [dt]
  (time/minus dt (time/days 1)))

(defn- future-dates [& {:keys [dt]}] (iterate inc-date (if-not dt (time/now) dt)))

(defn- prev-dates   [& {:keys [dt]}] (iterate dec-date (if-not dt (time/now) dt)))

(defn- battleday? [dt] (= 6 (time/day-of-week dt)))

(defn get-battleday
  "Return the a battledat interval, :prev for the latest one, and :next for the next one"
  ([& {:keys [next weeks] :or {next false weeks 1}}]
     (->> (time/now) (time/day-of-week)
          ((fn [weekday] (if next
                          (first (drop (dec weeks) (filter battleday? (future-dates ))))
                          (first (drop (dec weeks) (filter battleday? (prev-dates )))))))
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

(defn- stat-interval [stat]
  (time/interval (time-format/parse (:first-seen stat))
                 (time-format/parse (:last-seen stat))))

(defn- attended-battleday [& {:keys [stats weeks]}]
  (filter #(time/overlaps? (get-battleday :weeks weeks)
                           (stat-interval %)) stats))

(defn- get-active-users [stats]
  (->> stats (map :user) distinct (pmap #(->> % bl/get-username ))
       (filter #(not-empty %))))

;;TODO take used servers as list, alt check if bl can give private flag
(defn- roster-last-battleday ([] (roster-last-battleday (test-stats)))
  ([stats & {:keys [weeks] :or {weeks 1}}]
     (->> stats
          (mapcat #(for [stat (last %)]
                    (assoc stat :user (first %))))
          (attended-battleday :weeks weeks :stats)
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
