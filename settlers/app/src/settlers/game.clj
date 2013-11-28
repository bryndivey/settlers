(ns ^:shared settlers.game
    (:require [settlers.map :as map]))

;; map fetches

(defn tiles-for-vertex [g v]
  ;; return the tiles surrounding a vertex - eg, for initial resource allocation
  (let [faces (map/vertex-to-faces v)]
    (filter identity (map #(map/get-qr (:map g) %) faces))))

(defn tiles-for-roll [g r]
  (filter #(= (:roll %) r) (apply concat (:map g))))

;; dice

(defn roll-dice [n]
  (apply + (repeatedly n #(+ (rand-int 6) 1))))

(def r2d (partial roll-dice 2))


;; resource turn
; roll dice
; find tiles
; find settlements per tile

(defn tiles-with-settlements [g]
  "get a list of tuples of tiles and settlements"
  (for [[v s] (:vertices g)]
    [(map/vertex-to-faces v) s]))


(defn allocate-for-settlement-and-tile [g [s t]]
  "give the owner of s appropriate resources for t"
  (println "S T" s t)
  (let [r ({:mountain :ore
            :field :wheat
            :forest :wood
            :hill :brick
            :pasture :wool} (:terrain t))
        n (if (= (:type s) :settlement)
            1
            2)]
    (println "GIVING" (:owner s) n r)
    (update-in g [:players (:owner s) :resources r] + n)))

(defn allocate-resource [g t]
  "Takes a tile and a game and figures out who gets stuff"
  (let [tws (tiles-with-settlements g)
        pos (:position t)
        s-a-ts (for [[t' s] tws
]
                 [s t])]
    (println "SATS" s-a-ts)
    (reduce allocate-resource g s-a-ts)))

(defn resource-turn [g p]
  (let [r (r2d)
        t (tiles-for-roll g r)
        tws (tiles-with-settlements g)]
    (println "Roll" r)
    (println "Tiles" t)
    (map (partial allocate-resource g) t)
    ))




