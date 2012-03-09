(ns bf3.logic
  (:refer-clojure :exclude [==])
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem]
            [clj-time.core :as time]
            [clj-time.format :as time-format]
            [net.cgrand.enlive-html :as html]
            [bf3.stats :as stats]
            [bf3.bl :as bl])
  (:use [cheshire.core]
        [bf3.db]
        [clojure.core.logic])
  (:import java.net.URL))

(def ^{:dynamic true} *cache-time* (* 2 60 1000))

(def ^{:dynamic true} *debug* false)

(defn debug [& messages]
  (when *debug*
    (println messages)))

(defrel stat time server users info)

(defn- make-rules
  ([] (make-rules (bl/test-users)))
  ([info]
     (->> info
          (sort-by :time)
          (filter :info)
          (#(doseq [s %]
              (fact stat (:time s) (:server s) (:users s) (:info s)))))))

(defn roster
  ([] (roster (time/interval (time-format/parse "2012-03-06T18:00:00.000")
                             (time-format/parse "2012-03-07T05:00:00.000"))))
  ([interval]
     (->> (run* [q]
                (fresh [t s u i mm m g p uu]
                       (stat t s u {:mapMode mm :map m :gameId g :hasPassword p})
                       (project [t u p]
                                (== true (< 0 (count u)))
                                (== true  (true? p))
                                (== true (time/within? interval
                                                       (time-format/parse t)))
                                (== q [t s u {:mapMode mm :map m :gameId g :hasPassword p}] ))))
          (mapcat #(nth % 2))
          distinct
          (map bl/get-username)
          sort)))
