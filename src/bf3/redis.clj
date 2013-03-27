(ns bf3.redis
  (:require [taoensso.carmine :as car]))

(defn- get-redis-uri []
  (get (System/getenv) "REDISCLOUD_URL" "redis://"))

(def pool (car/make-conn-pool))
(def spec1 (car/make-conn-spec :uri (get-redis-uri)))

(defmacro wcar [& body] `(car/with-conn pool spec1 ~@body))

(defn set-data [key data]
  (do (wcar (car/ping)
            (car/set key data))))

(defn get-data [key]
  (wcar (car/get key)))
