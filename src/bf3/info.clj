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
                                              (first us))))))
               {:time {:start (->> (sort-by :time battle)
                                   first :time)
                       :end (->> (sort-by :time battle)
                                 last :time)}}
               {:live (->> (sort-by :time battle)
                           (map :live))}))

(defn- get-battle-infos []
  (->> (get-bl-users)
       (filter #(not-empty (:users %)))
       (sort-by :server)
       (partition-by :server)
       (mapcat (fn [x] (partition-by #(select-keys % [:info :gameId]) x)))
       (pmap get-battle-info)
       ))

(def battle-info (mem/memo-ttl get-battle-infos *cache-time*))

(defn- byte-to-char-val [bytes]
  (->> (if (coll? bytes) bytes [bytes])
       (map #(if (string? %) (Integer/parseInt %) %))
       (map (fn [n] (if (neg? n) (+ 256 n) n)))))

(defn- parse-hex [strings]
  (->> strings (map #(if (string? %) (Integer/parseInt % 16) %)) byte-to-char-val (map char)  (reduce str)))

(defn- charval? [n] (and (not= n 64) (> n 40)))

(defn- parse-players
  ([players] (parse-players players []))
  ([players res]
     (if (empty? players)
       res
       (let [name (parse-hex (take-while charval? players))
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
                  byte-to-char-val
                  (map #(Integer/toHexString %))
                  (map #(if (and (not= "0" %) (= 1 (count %)))
                          (str "0" %) %)))
        gameId (->> info (drop 6)
                    (take 4)
                    (reduce str)
                    (#(Integer/parseInt % 16)))
        postid (->> info (drop 11))
        gameMode (->> postid  (take-while #(not= "0" %))
                      (map #(char (Integer/parseInt % 16))) (reduce str))
        postmode (->> postid  (drop-while #(not= "0" %)))
        mapstart (if (re-find #"conquest" (s/lower-case gameMode))
                   (drop 13 postmode)
                   (if (re-find #"flag" (s/lower-case gameMode))
                     (->> postmode (drop-while #(not= "b0" %))
                          (drop 1) (drop-while #(not= "b0" %)) (drop 2))
                     []))
        currentMap (->> mapstart
                        (map #(Integer/parseInt % 16))
                        (take-while #(and (not= % 64) (> % 36)))
                        (map char)
                        (reduce str))
        postmap (->> mapstart
                        (map #(Integer/parseInt % 16))
                        (drop-while #(and (not= % 64) (> % 36))))
        playerstart (if (re-find #"conquest" (s/lower-case gameMode))
                      (drop 17 postmap)
                      (if (re-find #"flag" (s/lower-case gameMode))
                        (drop 15 postmap)
                     []))
        players (->> playerstart
                     parse-players)]
    (zipmap [:gameId :gameMode :currentMap :players] [gameId gameMode currentMap players])))
