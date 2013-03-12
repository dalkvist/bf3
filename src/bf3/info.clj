(ns bf3.info
  (:require [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:use [bf3.db]
        [bf3.bl :only [get-username get-user get-user-expansions]])
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
                                     (map (fn [us]
                                            (if (nil? (:clanTag (first us)))
                                              (let [tags (->> battle
                                                              (mapcat :users)
                                                              (filter #(= (:personaName (first us))
                                                                          (:personaName %) ))
                                                              (filter #(not (nil? (:clanTag %)))))]
                                                (if (not-empty tags)
                                                  (merge (first us) (select-keys (first tags) [:clanTag]))
                                                  (assoc  (first us) :clanTag "")))
                                              (first us)))))
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

(defn- parse-hex [strings]
  (reduce str (map #(char (if (string? %) (Integer/parseInt % 16) %)) strings)))

(defn- parse-players
  ([players] (parse-players players []))
  ([players res]
     (if (empty? players)
       res
       (let [charval? #(and (not= % 64) (> % 40))
             name (parse-hex (take-while charval? players))
             postname (drop-while charval? players)
             taglength (first postname)
             clanTags (if (= 0 taglength) "" (parse-hex (take taglength (drop 1 postname))))
             posttags (drop (inc taglength) postname)
             rank (first posttags)
             info (take 18 (drop 1 posttags))
             postinfo (drop 19 posttags)
             player (zipmap [:name :clanTags :rank :info] [name clanTags rank info] )]
         (recur postinfo (conj res player))))))

(defn parse-info [rawinfo]
  (let [info (->> rawinfo
                  (map #(Integer/parseInt (str %)))
                  (map (fn [n] (if (neg? n) (+ 256 n) n)))
                  (map #(Integer/toHexString %))
                  (map #(if (and (not= "0" %) (= 1 (count %)))
                          (str "0" %) %)))]
    {:gameId (->> info (drop 6)
                  (take 4)
                  (reduce str)
                  (#(Integer/parseInt % 16)))
     :gameMode (->> info (drop 11) (take-while #(not= "0" %))
                    (map #(char (Integer/parseInt % 16))) (reduce str))
     :currentMap (->> info (drop 11) (drop-while #(not= "b0" %))
                      (drop 1) (drop-while #(not= "b0" %)) (drop 2)
                      (map #(Integer/parseInt % 16))
                      (take-while #(and (not= % 64) (> % 36)))
                      (map char)
                      (reduce str))
     :players (->> info (drop 11) (drop-while #(not= "b0" %))
                      (drop 1) (drop-while #(not= "b0" %)) (drop 2)
                      (map #(Integer/parseInt % 16))
                      (drop-while #(and (not= % 64) (> % 36)))
                      (drop-while #(not= % 11))
                      (drop 1)
                      parse-players)}))
