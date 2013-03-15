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
                        "mongodb://user:pass@127.0.0.1:27017/bf3") ;; if not use localhost
          config    (split-mongo-url mongo-url)] ;; Extract options.
      (println "Initializing mongo @ " mongo-url)
      (mongo! :db (:db config) :host (:host config) :port (Integer. (:port config))) ;; Setup global mongo.
      (authenticate (:user config) (:pass config)) ;; Setup u/p.
      (doseq [coll [:ts-users :bl-logs :bl-users]]
        (or (collection-exists? coll);; Create collection if it doesn't exist.
            (do (create-collection! coll)
                (when (= coll ::bl-logs)
                  (add-index! :bl-logs [[:time -1]]) )))))))

(defn- clean-users [user]
  (dissoc user :_id))

(defn get-ts-users []
  (maybe-init)
  (map clean-users
       (fetch :ts-users)))

(defn save-ts-user! [ts-user]
  (maybe-init)
  (insert! :ts-users ts-user))

(defn get-bl-users
  ([] (get-bl-users true))
  ([descending]
     (maybe-init)
     (map clean-users
          (fetch :bl-logs :sort {:time (if descending -1 1)}
                 :where {:$and [ {"live" {:$ne []}}
                                 {"live" {:$ne nil}}
                                 {"users" {:$ne []}}]}))))

(defn get-battle
  [gameid & {:keys [start end descending]
               :or {start false end false descending true}}]
  (maybe-init)
  (let [gameid (if (string? gameid)
                 (Integer/parseInt gameid)
                 gameid)]
    (map clean-users
         (if (and end start)
           (fetch :bl-logs :sort {:time (if descending -1 1)}
                  :where {:$and  [
                                  {"live" {:$ne []}}
                                  {"users" {:$ne []}}
                                  {"gameId" gameid}
                                  {"time" {:$gte (str start)
                                           :$lte (str end)}}]})
           (fetch :bl-logs :sort {:time (if descending -1 1)}
                  :where {:$and  [
                                  {"live" {:$ne []}}
                                  {"users" {:$ne []}}
                                  {"gameId" gameid}]})))))

(defn get-log [serverid gameid ]
  (fetch-one :bl-logs :sort {:time -1}
             :where {:$and [ {"server" serverid}
                             {"gameId" gameid}]}))

(defn save-bl-user! [bl-log]
  (maybe-init)
  (let [old (get-log (:server bl-log) (:gameId (:info bl-log)))]
    (if (and (not-empty old) (apply = (map #(dissoc % :time :lastupdate :_id) [old bl-log])))
      (update! :bl-logs old (merge (assoc old :lastupdate (:time bl-log))))
      (insert! :bl-logs bl-log))))

(comment
  (maybe-init)
  (drop-coll! :bl-logs)
  (create-collection! :bl-logs)
  (add-index! :bl-logs [[:time -1]]))
