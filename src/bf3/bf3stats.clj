(ns bf3.bf3stats
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [clojure.core.cache :as cache]
            [clojure.core.memoize :as mem])
  (:use [cheshire.core]))

(declare *test-player-info* *test-weapond*)

(def player-url "http://api.bf3stats.com/pc/player/")

(def ^{:dynamic true} *underslig-weapons*
  ["AEK-971" "AK-74M" "AN-94" "F2000" "G3A3" "M16A3" "M16A4" "M416"])

(def ^{:dynamic true} *only-avalible?* true)
(def ^{:dynamic true} *only-main?* true)
(def ^{:dynamic true} *only-weapons?* true)

(def ^{:dynamic true} *player-cache-time* (* 2 60 1000))


(defn- player-info [player]
  "get the player info from bf3stats.com"
  (-> (client/get (str  player-url "/?player=" player "&opt=" ""))
                    :body
                    (parse-string true)))

(def get-player-info (mem/memo-ttl player-info *player-cache-time*))

(defn- get-class [class kits]
  "returns the class from kits"
  (get kits (keyword (s/lower-case class))))

(defn classes [player]
    "returns the available kit names for player"
  (->> player :stats :kits
       (filter #(= "kit" (:type (val %))))))

(defn- available-weapon-names [player class]
  "returns the unlocked weapon names for the class of player"
  (->> player
       :stats :kits (get-class class)
       :unlocks
       (filter #(and (= "weapon" (:type %))
                     (or (not (:curr %))
                         (>= (:curr %) (:needed % )))))))

(defn class-weapons
  "returns the weapons of the class for the player, only the unlocked ones if *only-avalible?*."
  ([player class]
     (let [exclude (concat (if *only-weapons?* ["Underslungs" "AT Launchers" "AA Launchers"] [])
                           (if *only-main?*["Pistols"] []))]
       (->> player
            :stats :weapons
            (filter #(and (if *only-avalible?*
                            (some (fn [w] (= (key %) (keyword (:id w))))
                                  (available-weapon-names player class))
                            true)
                          (= (s/lower-case class) (-> % val :kit s/lower-case))
                          (not-any? (set (->> % val :category s/lower-case list))
                                    (map s/lower-case exclude))))))))

(defn- attachment-slot [attachment]
  "return the attachment with the equitment slot it belongs to"
  ;;TODO fix PDW attachments
  (conj attachment
        {:slot
         (case (:name attachment)
           ("ACOG (4x)"  "PSO-1 (4x)" "KOBRA (RDS)" "PKA-S (HOLO)" "PKS-07 (7x)" "PK-A (3.4x)" "Ballistic (12x)"
            "Holographic (HOLO)" "Reflex (RDS)" "IRNV (IR 1x)" "Rifle Scope (6x)" "M145 (3.4x)" "Rifle Scope (8x)") :optical
            ("Foregrip" "Undersling Rail" "Straight Pull Bolt" "Bipod" "M320 BUCK" "M320 SMOKE") :primary
            ("Tactical Light" "Laser Sight"  "Flash Supp." "Suppressor" "Extended Mag" "Heavy Barrel"
              "12G Flechette" "12G Frag" "12G Slug") :secondary)}))

(defn attachments
  "returns the attchments for the weapon, only the unlocked ones if *only-avalible?*."
  ([weapon]
      (->> weapon val :unlocks
           (filter #(if *only-avalible?*
                      (or (not (:needed %))
                          (>= (:curr %) (:needed %)))
                      true))
           (#(if (some (fn [a] (= a (->> weapon val :name))) *underslig-weapons* )
               (conj % {:name "Undersling Rail" :id "undersling"})
               %))
           (map attachment-slot))))

(defn- random-attachments [weapon]
  "returns a random attachment for each attachment slot"
  (->> weapon attachments (sort-by :slot) (partition-by :slot)
       (map #(->> % shuffle first))))

(defn pistols
    "returns the pistols for the player, only the unlocked ones if *only-avalible?*."
  ([player]
      (->> (binding [*only-main?* false] (class-weapons player "general"))
           (filter #(and (if *only-avalible?*
                           (some (fn [w] (= (key %) (keyword (:id w))))
                                 (available-weapon-names player "general"))
                           true)
                         (= (s/lower-case "pistols")
                            (-> % val :category  s/lower-case)))))))

(defn- default-equipment [class]
  "returns the additional equitment for the class (starting equitment and mislabeled weapons)"
  (let [player *test-player-info*
        weapons (fn [cats]
                  (->> player :stats :weapons
                       (filter #(and (if *only-avalible?*
                                       (some (fn [w] (= (key %) (keyword (:id w))))
                                             (available-weapon-names player class))
                                       true)
                                     (some (set (->> % val :category keyword list))
                                           (map keyword cats))))
                        (map  val)))]
    (case (s/lower-case class)
      "assault" (concat (weapons ["Underslungs"])
                       [{:type "kititem" :name "Medic kit" :id "medkit"}])
      "engineer" (concat (weapons ["AT Launchers" "AA Launchers"])
                        [{:type "kititem" :name "RPG-7V2" :id "rpg"}
                         {:type "kititem" :name "SMAW" :id "smaw"}
                         {:type "kititem" :name "Repair Tool" :id "repair"}])
      "support" [{:type "kititem" :name "Ammo Box" :id "ammobox"}]
      "recon" [{:type "kititem" :name "Radio Beacon" :id "beacon"}])))

(defn- equipment-slot [equipment]
  "adds information on equipments slot"
  (conj equipment
        {:slot
         (case (:name equipment)
           ("FIM-92 STINGER" "FGM-148 JAVELIN" "SA-18 IGLA" "SMAW" "RPG-7V2" "M320" "M26 MASS"
            "Ammo Box" "Medic kit" "T-UGS" "SOFLAM" "MAV") :gadget1
           ("DEFIBRILLATOR" "Repair Tool" "EOD BOT" "M15 AT MINE" 
            "C4 EXPLOSIVES" "M18 CLAYMORE" "M224 MORTAR" "Radio Beacon" ) :gadget2)}))

(defn equipment
  "returns the equipment of the class for the player, only the unlocked ones if *only-avalible?*."
  ([player class]
     (let [c (->> player :stats :kits ((fn [k] (get-class class k))))]
       (->> c :unlocks 
            (filter #(and (= "kititem" (:type %))
                          (if *only-avalible?*
                            (or (not (:needed %))
                                (>= (:curr %) (:needed %)))
                            true)))
            (concat (default-equipment class))
            (map equipment-slot)))))

(defn- random-equipment [player class]
  "returns a random equipment for each equipment slot"
  (->> (equipment player class) (sort-by :slot) (partition-by :slot)
       (map #(->> % shuffle first))))

(defn specializations [player]
  "returns the specializations for the player, only the unlocked ones if *only-avalible?*."
  (->> player :stats :specializations
       (filter #(if *only-avalible?*
                  (or (not (:needed (val %)))
                      (>= (:curr (val %)) (:needed (val %))))
                  true))
       ((fn [list] (filter #(if (= "2" (->> % key str last str))
                             true
                             (if (some (fn [k] (= (->> k key s/lower-case butlast (reduce str))
                                                 (->> % key s/lower-case)))
                                       list)
                               false true))
                          list)))))

(defn random-loadout
  ([] (binding [*only-avalible?* false] (random-loadout *test-player-info*)))
  ([player] (let [player (if (string? player) (get-player-info player) player)
                  class (->> player classes shuffle first val :name)
                  weapon (->> (class-weapons player class) shuffle first)
                  attachments (->> (random-attachments weapon) (map :name))]
              (hash-map :class class
                        :weapon {:name (->> weapon val :name)
                                 :attachments attachments}
                        :sidearm (->> (pistols player) shuffle first val :name)
                        :equipment (->> (random-equipment player class) (map :name))
                        :specialization (->> player specializations shuffle first val :name)))))


(def ^{:dynamic true } *test-player-info*
                (get-player-info "cairdazar"))

(def ^{:dynamic true} *test-weapond*
  (->> (class-weapons *test-player-info* "assault") first))
