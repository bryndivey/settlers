(ns ^:shared settlers.game
    (:require [settlers.map :as map]
              [settlers.actions :refer [perform-move valid-move?]]
              [settlers.create :as create]
              [settlers.utils :as utils]))

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
      :field :grain
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

    (update-in g [:players (:player s) :resources r] + n)))

(defn allocate-for-tile [g t]
  "Takes a tile and a game and figures out who gets stuff"
  (let [pos (:position t)
        s-and-ts (for [[p s] (tiles-with-settlements g)
                       :when (= pos p)]
                   [s t])]
    (reduce allocate-for-settlement-and-tile g s-and-ts)))

(defn do-resource-allocation [g r]
  "Takes a game and a roll and does resources"
  (update-in 
      (reduce allocate-for-tile g (tiles-for-roll g r))
      [:moves] conj {:type :roll :roll r}))

(defn resource-roll [g]
  (let [r (r2d)]
    (do-resource-allocation g r)))



(defn game-faces [g]
  "All faces in play in the game g"
  (map :position (filter identity (apply concat (:map g)))))


(defn valid-action [t a]
  "Is this action valid in the current move type?"
  (let [v-map {:game-action #{:build-road
                              :build-settlement
                              :build-city
                              :buy-development-card
                              :end-turn}}]
    (boolean ((v-map t) a))))

(defn next-player [g p]
  (second (utils/rotate-while #(not= p %) (:player-order g))))

(defn game-loop
  ([g] g)
  ([g m]
     (let [action (:action m)]
       (if (valid-action (-> g :next-move :type) action)
         (if (valid-move? g m)
           (cond
            (= action :end-turn) (-> g
                                     resource-roll
                                     (assoc :next-move {:player
                                                        (next-player g (:player (:next-move g)))
                                                        :type :game-action}))
            :else  (perform-move g m))
           (assoc g :error "Invalid move"))
         (assoc g :error "Invalid move")))))

(defn create []
  (-> (create/create-game)
      (create/add-player "Bryn")
      (create/add-player "Mark")
      
      (create/add-settlement :bryn [[0 0] :n])
      (create/add-road :bryn #{[0 0] [-1 0]})
      (create/add-settlement :bryn [[2 -2] :n])
      (create/add-road :bryn #{[1 -2] [1 -1]})
      
      (create/add-settlement :mark [[1 1] :n])
      (create/add-road :mark #{[1 1] [0 1]})
      (create/add-settlement :mark [[-1 0] :w])
      (create/add-road :mark #{[-1 0] [-2 0]})))




