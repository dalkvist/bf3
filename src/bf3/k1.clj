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

(def ^{:dynamic true} *cache-time* (* 2 60 1000))


(defn- k1-info [player]
  "get the k1 info from the google document"
  )

(def get-k1-info (mem/memo-ttl k1-info *cache-time*))
