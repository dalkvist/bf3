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
             clanTags (if (= 0 taglength) "" (str (parse-hex (take taglength (drop 1 postname)))))
             posttags (drop (inc taglength) postname)
             rank (first posttags)
             info (let [info (take 14 (drop 1 posttags))]
                     (merge (->> info
                                   (map #(Integer/toHexString %))
                                   (#(vector (take 4 %) (take 2 (drop 4 %)) (take 2 (drop 6 %))))
                                   (map #(reduce str %))
                                   (map #(Integer/parseInt % 16))
                                   (zipmap [:score :kills :deaths]))
                              {:squad (nth info 8)}))
             postinfo (drop 14 posttags)
             player (merge info {:team (if (> players-team1 (count res)) :1 :2)}
                           (zipmap [:personaName :clanTags :rank :personaId] [name clanTags rank personaId] ))]
         (recur players-team1 players-team2 postinfo (conj res player))))))

(defn- get-stats [score users]
  (if (or (nil? users) (empty? users))
     score
    (->> (merge-with merge (->> users (partition-by :team)
                                (map (fn [t] (hash-map (:team (first t))
                                                      (->> t (map #(select-keys % [:kills :deaths]))
                                                           (apply merge-with +)))))
                                (reduce merge))
                     score)
         (#(if (= 2 (count (keys %)))
             (->> (merge-with merge % (apply hash-map (mapcat (fn [[t o]]
                                                                (vector
                                                                 (nth (keys %) t)
                                                                 (hash-map :revives
                                                                           ((fn [n] (if (neg? n) 0 n))
                                                                            (- (:kills (nth (vals %) o))
                                                                               (:deaths (nth (vals %) t )))))))
                                                              [[0 1] [1 0]])))
                  (map (fn [m] (hash-map (first m) (assoc (second m) :bleed ((fn [n] (if (neg? n) 0 n))
                                                                            (- (:max (second m))
                                                                               (:current (second m))
                                                                               (:deaths (second m))))))))
                  (apply merge))
             %)))))

(defn parse-rawinfo [rawinfo]
  (try
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
          score (try (let [l (Integer/parseInt (first (drop 1 postmode)) 16)
                           infos (->> postmode (drop 2) (take l))]
                       {:score (if (re-find #"squaddeath" (s/lower-case gameMode))
                                 (->> infos (partition-all 5)
                                      (map (fn [t] (vector (keyword (str (Integer/parseInt (first t))))
                                                          (->> (rest t) (partition-all 2)
                                                               (map #(reduce str %))
                                                               (map #(Integer/parseInt % 16))
                                                               (zipmap [:current :max])))))
                                      flatten (apply hash-map))
                                 (->> infos
                                      (#(if (re-find #"rush" (s/lower-case gameMode))
                                          (vector (take 2 (drop 2 %)) (take 2 (drop 4 %))
                                                  (take 1 (drop (+ 2 (/ l 2)) %))
                                                  (take 1 (drop (+ 3 (/ l 2)) %)))
                                          (vector (take 2 (drop 1 %)) (take 2 (drop 3 %))
                                                  (take 2 (drop (+ 1 (/ l 2)) %))
                                                  (take 2 (drop (+ 3 (/ l 2)) %)))))
                                      (map #(reduce str %))
                                      (map #(Integer/parseInt % 16))
                                      (partition-all 2)
                                      (map #(zipmap [:current :max ] %))
                                      (zipmap [:1 :2])))})
                     (catch Exception e))
          postinfo (->> postmode  (drop (+ 2 (Integer/parseInt (first (drop 1 postmode)) 16))))
          currentMap (->> postinfo
                          (drop 1) (take (Integer/parseInt (first postinfo) 16))
                          parse-hex
                          (reduce str))
          postmap (->> postinfo (drop (inc (Integer/parseInt (first postinfo) 16))))
          maxplayers (Integer/parseInt  (first postmap) 16)
          players-team1 (if-let [n (first (drop 4 postmap))]
                          (Integer/parseInt n  16) 0)
          players-team2 (if-let [n (first (drop 5 postmap))]
                          (Integer/parseInt n  16) 0)
          playerstart (drop 12 postmap)
          players (try (->> playerstart
                            (map #(Integer/parseInt % 16))
                            (parse-players players-team1 players-team2))
                       (catch Exception  e))
          stats (try (get-stats (:score score) players)
                     (catch Exception e))]
      (zipmap [:gameId :gameMode :currentMap :maxplayers :users  :mapVariant :stats]
              [gameId gameMode currentMap maxplayers players mapvariant stats]))
    (catch Exception e)))

(def parse-info (mem/memo-ttl parse-rawinfo *cache-time*))

(defn- get-battle-info [battle]
  (merge (select-keys (first battle) [:server])
               (:info   (first battle))
               (hash-map :users (->> (if (some #(not (nil? %)) (map :lve battle))
                                       (->> battle (mapcat #(->> % :live parse-info :players))
                                            (map #(select-keys % [:personaId :personaName :clanTags])))
                                       (mapcat :users battle))
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
               {:live (->> battle
                           (map :live))}))

(defn- parse-battle-infos
  ([logs]
     (->> logs
          (filter #(not-empty (:users %)))
          (sort-by :server)
          (partition-by :server)
          (mapcat (fn [x] (partition-by #(select-keys % [:info :gameId]) x))))))

(defn- get-battle-infos
  ([] (get-battle-infos (get-bl-users)))
  ([logs]
     (pmap get-battle-info (parse-battle-infos logs))))

(def battle-info (mem/memo-ttl get-battle-infos *cache-time*))

(defn merge-infos [infos]
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
