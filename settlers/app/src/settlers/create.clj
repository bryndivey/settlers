(ns ^:shared settlers.create
    (:require [settlers.map :refer [set-qr]]))

(defn create-player [name]
  (let [id (keyword (clojure.string/lower-case name))]
    {:id id
     :name name
     :resources {:wood 0
                 :ore 0
                 :grain 0
                 :sheep 0
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
(defn create-map []
  (let [map (vec (repeat 5 (vec (repeat 5 nil))))
        lfn (fn [m [next-pos & ps] [next-terrain & ts] rs]
              (if (nil? next-pos)
                m
                (let [desert (= next-terrain :desert)
                      next-roll (if desert nil (first rs))
                      rs (if desert rs (rest rs))]
                  (recur (set-qr m next-pos {:terrain next-terrain
                                             :roll next-roll})
                         ps ts rs))))]
    (lfn map positions (shuffle terrains) rolls)))


(defn create-game []
  {:map (create-map)
   :players {}
   :player-order []})

(defn add-player [g name]
  (let [p (create-player name)]
    (-> g 
        (assoc-in [:players (:id p)] p)
        (update-in [:player-order] conj (:id p)))))