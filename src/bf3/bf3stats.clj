(ns bf3.bf3stats
  (:require [clj-http.client :as client]
            [clojure.string :as s])
  (:use [cheshire.core]))

(def player-url "http://api.bf3stats.com/pc/player/")

(defn- get-player-info [player]
  (parse-string  (client/post player-url {:player player}))
  )

(def ^{:dynamic true } *test-player-info*
                (-> (client/get (str  player-url "/?player=" "cairdazar" "&opt=" ""))
                    :body
                    (parse-string true)))

(def ^{:dynamic true} *test-weapond*
  (->> *test-player-info*
       :stats :weapons :caSG553))

(defn kits [player]
  (->> player :stats :kits
       (filter #(= "kit" (:type (val %))))
       (map #(-> % val :name))))

(defn- available-weapon-names [player kit]
  (->> player
       :stats :kits (#(get % (keyword kit)))
       :unlocks
       (filter #(and (= "weapon" (:type %))
                     (or (not (:curr %))
                         (>= (:curr %) (:needed % )))))))

(defn kit-weapons
  ([player kit] (kit-weapons player kit true))
  ([player kit only-avalible?]
      (->> player
           :stats :weapons
           (filter #(and (if only-avalible?
                           (some (fn [w] (= (key %) (keyword (:id w))))
                                 (available-weapon-names player kit))
                           true)
                     (= (s/lower-case kit) (-> % val :kit s/lower-case)))))))
