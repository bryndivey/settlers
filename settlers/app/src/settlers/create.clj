(ns ^:shared settlers.create
    (:require [settlers.map :refer [set-qr]]))

(defn create-player [name]
  (let [id (keyword (clojure.string/lower-case name))]
    {:id id
     :name name
     :resources {:wood 0
                 :ore 0
                 :wheat 0
                 :wool 0
                 :brick 0}
     :hand []
     :cards []
     }))

(def terrains
  (reduce into []
          (map (partial apply repeat)
               [[1 :desert]
                [3 :mountain]
                [3 :hill]
                [4 :field]
                [4 :forest]
                [4 :pasture]])))

(def cards
  (reduce into []
          (map (partial apply repeat)
               [[14 {:type :knight}]
                [2 {:type :road-building}]
                [2 {:type :year-of-plenty}]
                [2 {:type :monopoly}]
                [5 {:type :victory-point}]])))

;; TODO - generate spiral
(def positions [[0 -2]
                [1 -2]
                [2 -2]
                [2 -1]
                [2  0]
                [1  1]
                [0  2]
                [-1 2]
                [-2 2]
                [-2 1]
                [-2 0]
                [-1 -1]
                [0 -1]
                [1 -1]
                [1  0]
                [0  1]
                [-1 1]
                [-1 0]
                [0  0]])

(def rolls [5 2 6 3 8 10 9 12 11 4 8 10 9 4 5 6 3 11])


;; TODO - hardcoded vals
(defn -create-map [terrains]
  (let [map (vec (repeat 5 (vec (repeat 5 nil))))
        lfn (fn [m [next-pos & ps] [next-terrain & ts] rs]
              (if (nil? next-pos)
                m
                (let [desert (= next-terrain :desert)
                      next-roll (if desert nil (first rs))
                      rs (if desert rs (rest rs))]
                  (recur (set-qr m next-pos {:terrain next-terrain
                                             :roll next-roll
                                             :position next-pos})
                         ps ts rs))))]
    (lfn map positions terrains rolls)))


(defn -create-game [terrains cards]
  {:map (-create-map terrains)
   :vertices {}
   :edges {}
   :moves []
   :players {}
   :player-order []
   :cards cards
   :next-move nil})

(defn create-game []
  (-create-game (shuffle terrains) (shuffle cards)))

(defn add-player
  ([g name]
      (let [p (create-player name)]
        (-> g 
            (assoc-in [:players (:id p)] p)
            (update-in [:player-order] conj (:id p))
            (assoc :next-move {:player (:id p) :type :game-move})))))

(defn add-settlement [game player vertex]
  (assoc-in game [:vertices vertex] {:type :settlement
                                     :position vertex
                                     :player player}))

(defn add-road [game player edge]
  (assoc-in game [:edges edge] {:type :road
                                :position (set edge)
                                :player player}))

(defn add-city [game player vertex]
  (assoc-in game [:vertices vertex] {:type :city
                                     :position vertex
                                     :player player}))

