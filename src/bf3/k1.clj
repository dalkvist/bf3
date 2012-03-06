(ns bf3.k1
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem])
  (:use [cheshire.core])
  (:import java.net.URL
           (com.gargoylesoftware.htmlunit BrowserVersion HttpMethod Page RefreshHandler WebClient WebRequestSettings)
           (com.gargoylesoftware.htmlunit.html HtmlAnchor HtmlForm HtmlPage HtmlTable HtmlTableRow)))

(def ^:dynamic client (ref (new WebClient)))

(.setThrowExceptionOnScriptError @client false)
(.setThrowExceptionOnFailingStatusCode @client false)


(def api-url "https://spreadsheets.google.com/feeds/download/spreadsheets/Export?key=0AolGselmDdqedDhnMkZGNXdZQWFON3JoTjd0WGs3c0E&exportFormat=csv")

(def k1-url "https://docs.google.com/spreadsheet/ccc?key=0AolGselmDdqedDhnMkZGNXdZQWFON3JoTjd0WGs3c0E")

(def k1-url-2 "https://docs.google.com/spreadsheet/lv?key=0AolGselmDdqedDhnMkZGNXdZQWFON3JoTjd0WGs3c0E")

(def ^{:dynamic true} *cache-time* (* 30 60 1000))

(defstruct player :forum-name :origin-name :rank :branch :jet :halo :armor)

(defn- get-player-info [_ f-name o-name rank branch _ jet halo armor _]
  (struct player f-name o-name rank branch (not (empty? jet)) (not (empty? halo)) (not (empty? armor))))

(defn- get-k1-players []
  (->> (.getByXPath (.getPage @client k1-url-2)
        "//table[@id='tblMain']//tr") (drop 1)
       (map (fn [row] (->>  row .getCells (map #(.asText %)) (apply get-player-info))))))

(def k1-players (mem/memo-ttl get-k1-players *cache-time*))

(defn- k1-info [player-name]
  "get the k1 info from the google document"
  (first (filter #(or  (= (s/lower-case player-name) (s/lower-case (:forum-name %)))
                       (= (s/lower-case player-name) (s/lower-case (:origin-name %))))
                 (k1-players))))

(def get-k1-info (mem/memo-ttl k1-info *cache-time*))
