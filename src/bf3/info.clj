(ns bf3.info
  (:require [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use [bf3.db]
        [bf3.bl :only [server-ids]])
  (:import java.net.URL))


(def ^{:dynamic true} *cache-time* (* 2 60 1000))
(def ^{:dynamic true} *short-cache-time* (* 30 1000))
(def ^{:dynamic true} *long-cache-time* (* 60 60 1000))

(defn merge-live-infos [infos]
  (reduce (fn [l1 l2]
            (let [m (merge l1 l2)
                  online (fn [u t] (if (not-empty u) (assoc u :online t) u))
                  u2 (map #(online % true) (get l2 :users []))
                  u2-ids (into #{} (map :personaId u2))
                  u1-ids (map :personaId (get l1 :users []))
                  u1 (->> (get l1 :users [])
                          (filter (fn [u] (not (contains? u2-ids (:personaId u)))))
                          (map #(online % false)))]
              (merge m {:users
                        (filter not-empty
                                (concat [] u1 u2))})))
          infos))

 (def merge-infos (mem/memo-ttl merge-live-infos *long-cache-time*))

(defn- get-battle-info [battles]
  (when (and  (not-empty battles)
              (map? (first battles)))
    (let [live-infos (map :live battles)
          mergedinfo (merge-infos live-infos)
          lastb (last battles)
          start (if (:lastupdate lastb) (:lastupdate lastb)
                  (:time lastb))
          end (->> battles first :time)]
      (merge (first battles)
             (when (map? mergedinfo) mergedinfo)
             {:time {:start start
                     :end  end}
              :live live-infos}))))

(defn- parse-battle-infos
  ([logs]
     (->> logs
          (filter (fn [b] (and (not-empty (:users b)))
                    (some (fn [[team score]] (not= 0 (:max score))) (:stats b))))

          (#(partition-by
             (fn [b] (str (:gameId b) (:currentMap b) (:gameMode b) (:mapVariant b)
                         (:vehicles b) (:mapMode b)
                         (some (fn [[t s]] (apply = (vals (select-keys s [:max :current]))))
                               (:stats b)))) %))
          (filter #(< 15 (count %))))))

(comment


  (some (fn [[team score]]
          (and (re-find #"conquest" (s/lower-case (str (:name b))))
               (or (= 0 (:current score))
                   (= 1 (:current score)))))
        (:stats b)))

(defn- get-battles
  ([logs]
     (pmap get-battle-info (parse-battle-infos logs))))

(defn- get-battle-infos
  ([& {:keys [weeks logs] :or {weeks 0}}]
     (if logs (get-battles logs)
       (pmap (fn [id] (filter #(not-empty (:live %))
                             (get-battles (get-battles-by-server id :weeks weeks))))
             (vals server-ids)))))

(def battle-info (mem/memo-ttl get-battle-infos *short-cache-time*))
