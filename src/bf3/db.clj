(ns bf3.db
  (:use [somnium.congomongo]
        [somnium.congomongo.config :only [*mongo-config*]]))

(defn- split-mongo-url [url]
  "Parses mongodb url from heroku, eg. mongodb://user:pass@localhost:1234/db"
  (let [matcher (re-matcher #"^.*://(.*?):(.*?)@(.*?):(\d+)/(.*)$" url)] ;; Setup the regex.
    (when (.find matcher) ;; Check if it matches.
      (zipmap [:match :user :pass :host :port :db] (re-groups matcher))))) ;; Construct an options map.

(defn- maybe-init []
  "Checks if connection and collection exist, otherwise initialize."
  (when (not (connection? *mongo-config*)) ;; If global connection doesn't exist yet.
    (let [mongo-url (or (get (System/getenv) "MONGOLAB_URI")            ;; Heroku puts it here.
                        "mongodb://user:pass@localhost:27017/bf3") ;; if not use localhost
          config    (split-mongo-url mongo-url)] ;; Extract options.
      (println "Initializing mongo @ " mongo-url)
      (mongo! :db (:db config) :host (:host config) :port (Integer. (:port config))) ;; Setup global mongo.
      (authenticate (:user config) (:pass config)) ;; Setup u/p.
      (doseq [coll [:ts-users :bl-users]]
        (or (collection-exists? coll);; Create collection if it doesn't exist.
            (do (create-collection! coll)
                ;; (when (= coll :traderausers)
                ;;   (mass-insert! coll default-users))
                )))
      )))

(defn- clean-users [user]
  (select-keys user (remove #(= :_id %) (keys user))))

(defn get-ts-users []
  (maybe-init)
  (map clean-users
       (fetch :ts-users)))

(defn save-ts-user! [ts-user]
  (maybe-init)
  (insert! :ts-users ts-user))

(defn get-bl-users []
  (maybe-init)
  (map clean-users
       (fetch :bl-users)))

(defn save-bl-user! [bl-user]
  (maybe-init)
  (insert! :bl-users bl-user))
