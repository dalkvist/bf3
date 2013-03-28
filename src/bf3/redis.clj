(ns bf3.redis
  (:require [taoensso.carmine :as car]))

(defn- get-redis-uri []
  (get (System/getenv) "REDISCLOUD_URL"))

(defmacro wcar [& body] `(car/with-conn (car/make-conn-pool)
                             (car/make-conn-spec :uri (get-redis-uri))
                           ~@body))

(defn set-data [key data]
  (do (wcar (car/ping)
            (car/set key data))))

(defn get-data [key]
  (wcar (car/get key)))
