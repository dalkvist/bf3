(ns bf3.bf3stats
  (:require [clj-http.client :as client]
            [clojure.string :as s])
  (:use [cheshire.core]))

(def player-url "http://api.bf3stats.com/pc/player/")

(def ^{:dynamic true} *underslig-weapons*
  ["AEK-971" "AK-74M" "AN-94" "F2000" "G3A3" "M16A3" "M16A4" "M416"])

(def ^{:dynamic true} *only-avalible?* true)
(def ^{:dynamic true} *only-main?* true)
(def ^{:dynamic true} *only-weapons?* true)

(defn- get-player-info [player]
  (-> (client/get (str  player-url "/?player=" player "&opt=" ""))
                    :body
                    (parse-string true)))

(def ^{:dynamic true } *test-player-info*
                (get-player-info "cairdazar"))

(def ^{:dynamic true} *test-weapond*
  (->> *test-player-info*
       :stats :weapons :caSG553))

(defn- get-class [class kits]
  (get kits (keyword (s/lower-case class))))

(defn classes [player]
    "returns the available kit names for player"
  (->> player :stats :kits
       (filter #(= "kit" (:type (val %))))))

(defn- available-weapon-names [player class]
  "returns the unlocked weapon names for the kit of player"
  (->> player
       :stats :kits (get-class class)
       :unlocks
       (filter #(and (= "weapon" (:type %))
                     (or (not (:curr %))
                         (>= (:curr %) (:needed % )))))))

(defn kit-weapons
  "returns the weapons of the kit for the player, only the unlocked ones if *only-avalible?*."
  ([player kit]
      (->> player
           :stats :weapons
           (filter #(and (if *only-avalible?*
                           (some (fn [w] (= (key %) (keyword (:id w))))
                                 (available-weapon-names player kit))
                           true)
                         (= (s/lower-case kit) (-> % val :kit s/lower-case))
                         (not-any? (set (->> % val :category keyword list))
                                   (map keyword (concat (if *only-weapons?*
                                                          ["Underslungs" "AT Launchers" "AA Launchers"]
                                                          [] )
                                                        (if *only-main?* ["Pistols"] [])))))))))

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
  (->> weapon attachments (sort-by :slot) (partition-by :slot)
       (map #(->> % shuffle first))))

(defn pistols
    "returns the pistols for the player, only the unlocked ones if *only-avalible?*."
  ([player]
      (->> (kit-weapons player "general" false)
           (filter #(and (if *only-avalible?*
                           (some (fn [w] (= (key %) (keyword (:id w))))
                                 (available-weapon-names player "general"))
                           true)
                         (= (s/lower-case "pistols")
                            (-> % val :category  s/lower-case)))))))

(defn- default-equipment [class]
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
    (case (keyword class)
      :assault (concat (weapons ["Underslungs"])
                       [{:type "kititem" :name "Medic kit" :id "medkit"}])
      :engineer (concat (weapons ["AT Launchers" "AA Launchers"])
                        [{:type "kititem" :name "RPG-7V2" :id "rpg"}
                         {:type "kititem" :name "SMAW" :id "smaw"}
                         {:type "kititem" :name "Repair Tool" :id "repair"}])
      :support [{:type "kititem" :name "Ammo Box" :id "ammobox"}]
      :recon [{:type "kititem" :name "Radio Beacon" :id "beacon"}])))

(defn- equipment-slot [equipment]
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


















