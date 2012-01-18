(ns bf3.db
  (:use [somnium.congomongo]
	[somnium.congomongo.config :only [*mongo-config*]]))

(defn split-mongo-url [url]
  "Parses mongodb url from heroku, eg. mongodb://user:pass@localhost:1234/db"
  (let [matcher (re-matcher #"^.*://(.*?):(.*?)@(.*?):(\d+)/(.*)$" url)] ;; Setup the regex.
    (when (.find matcher) ;; Check if it matches.
      (zipmap [:match :user :pass :host :port :db] (re-groups matcher))))) ;; Construct an options map.

(defn maybe-init []
  "Checks if connection and collection exist, otherwise initialize."
  (when (not (connection? *mongo-config*)) ;; If global connection doesn't exist yet.
    (let [mongo-url (or (get (System/getenv) "MONGOLAB_URI")            ;; Heroku puts it here.
			"mongodb://user:pass@localhost:27017/bf3") ;; if not use localhost 
	  config    (split-mongo-url mongo-url)] ;; Extract options.
      (println "Initializing mongo @ " mongo-url)
      (mongo! :db (:db config) :host (:host config) :port (Integer. (:port config))) ;; Setup global mongo.
      (authenticate (:user config) (:pass config)) ;; Setup u/p.
;;       (doseq [coll [:apps :users :stats :traderausers]]
;; 	(or (collection-exists? coll);; Create collection if it doesn't exist.
;; 	    (do (create-collection! coll)
;; 		(when (= coll :traderausers)
;; 		  (mass-insert! coll default-users)))))
      )))

(defn get-traderauser [name]
  (maybe-init)
  (fetch-one :traderausers :where {:name name}))

(defn get-traderausers []
  (maybe-init)
  (fetch :traderausers))

(defn get-apps []
  (maybe-init)
  (fetch :apps))

(defn get-app [id]
  (maybe-init)
  (fetch-one :apps :where {:appid id}))

(defn save-app! [app]
  (maybe-init)
  (if-let [old-app (get-app (:appid app))]
    (update! :apps old-app app)	;if exists upate  
    (insert! :apps app)))       ;else insert new

(defn get-stat [{id :appid time :time method :method}]
  (maybe-init)
  (fetch-one :stats :where {:appid id :time time :method method}))

(defn save-stat! [stat]
  (maybe-init)
  (if-let [old-stat (get-stat stat)]
    (update! :stats old-stat stat)	;if exists upate  
    (insert! :stats stat)))       ;else insert new
