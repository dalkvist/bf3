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
  ([players-team1 players-team2 players]
     (parse-players players-team1 players-team2 players []))
  ([players-team1 players-team2 players res]
     (if (empty? players)
       res
       (let [personaId (->> players  (take 4)
                            (map #(Integer/toHexString %))
                            (reduce str)
                            (#(Integer/parseInt % 16)))

             postid  (->> players (drop 4))
             name (parse-hex (take  (first postid) (drop 1 postid)))
             postname  (drop (inc (first postid)) postid)
             taglength (first postname)
             clanTags (if (= 0 taglength) "" (parse-hex (take taglength (drop 1 postname))))
             posttags (drop (inc taglength) postname)
             rank (first posttags)
             info (let [info (take 14 (drop 1 posttags))]
                     (merge (->> info
                                   (map #(Integer/toHexString %))
                                   (#(vector (take 4 %) (take 2 (drop 4 %)) (take 2 (drop 6 %))))
                                   (map #(reduce str %))
                                   (map #(Integer/parseInt % 16))
                                   (zipmap [:score :kill :deaths]))
                              {:squad (nth info 8)}))
             postinfo (drop 14 posttags)
             player (merge info {:team (if (> players-team1 (count res)) :1 :2)}
                           (zipmap [:name :clanTags :rank :personaId] [name clanTags rank personaId] ))]
         (recur players-team1 players-team2 postinfo (conj res player))))))



(defn parse-info [rawinfo]
  (let [info (->> rawinfo
                  (map #(Integer/parseInt (str %)))
                  byte-to-char-val
                  (map #(Integer/toHexString %))
                  (map #(if (and (not= "0" %) (= 1 (count %)))
                          (str "0" %) %)))
        gameId (->> info (drop 2)
                    (take 8)
                    (reduce str)
                    (#(Integer/parseInt % 16)))
        postid (->> info (drop 10))
        gameMode (->> postid (drop 1) (take (Integer/parseInt (first postid) 16))
                      (map #(char (Integer/parseInt % 16))) (reduce str))
        postmode (->> postid  (drop (inc (Integer/parseInt (first postid) 16))))
        mapvariant (first postmode)
        score (let [infos (->> postmode (drop 2) (take (Integer/parseInt (first (drop 1 postmode)) 16)))]
                {:score (->> infos
                             (#(vector (take 2 (drop 1 %)) (take 2 (drop 3 %))
                                       (take 2 (drop (+ 1 (/ (Integer/parseInt
                                                              (first (drop 1 postmode)) 16) 2)) %))
                                       (take 2 (drop (+ 3 (/ (Integer/parseInt
                                                              (first (drop 1 postmode)) 16) 2)) %))))
                             (map #(reduce str %))
                             (map #(Integer/parseInt % 16))
                             (partition-all 2)
                             (map #(zipmap [:current :max ] %))
                             (zipmap [:1 :2]))})
        postinfo (->> postmode  (drop (+ 2 (Integer/parseInt (first (drop 1 postmode)) 16))))
        currentMap (->> postinfo
                        (drop 1) (take (Integer/parseInt (first postinfo) 16))
                        parse-hex
                        (reduce str))
        postmap (->> postinfo (drop (inc (Integer/parseInt (first postinfo) 16))))
        maxplayers (Integer/parseInt  (first postmap) 16)
        players-team1 (Integer/parseInt  (first (drop 4 postmap)) 16)
        players-team2 (Integer/parseInt  (first (drop 5 postmap)) 16)
        playerstart (drop 12 postmap)
        players (->> playerstart
                     (map #(Integer/parseInt % 16))
                     (parse-players players-team1 players-team2))]
    (merge score (zipmap [:gameId :gameMode :currentMap :maxplayers :players  :mapVariant]
                         [gameId gameMode currentMap maxplayers players  mapvariant]))))
