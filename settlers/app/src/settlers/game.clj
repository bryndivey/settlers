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
  "return a list of tiles "
  (for [[v s] (:vertices g)
        f (map/vertex-to-faces v)]
    [f s]))

(defn resource-for-terrain [g t]
  (t {:mountain :ore
      :field :wheat
      :forest :wood
      :hill :brick
      :pasture :wool}))

(defn num-resources-for-settlement [g s]
  ((:type s) {:settlement 1
              :city 2}))

(defn allocate-for-settlement-and-tile [g [s t]]
  "give the owner of s appropriate resources for t"
  (let [r (resource-for-terrain g (:terrain t))
        n (num-resources-for-settlement g s)]
    (assert r (str "Invalid terrain" (:terrain t)))
    (assert n (str "Invalid settlement" s))

    (update-in g [:players (:owner s) :resources r] + n)))

(defn allocate-for-tile [g t]
  "Takes a tile and a game and figures out who gets stuff"
  (let [pos (:position t)
        s-and-ts (for [[p s] (tiles-with-settlements g)
                       :when (= pos p)]
                   [s t])]
    (reduce allocate-for-settlement-and-tile g s-and-ts)))

(defn do-resource-allocation [g r]
  "Takes a game and a roll and does resources"
  (reduce allocate-for-tile g (tiles-for-roll g r)))

(defn resource-turn [g p]
  (let [r (r2d)]
    (do-resource-allocation g r)))




