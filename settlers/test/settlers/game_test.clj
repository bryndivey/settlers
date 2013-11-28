(ns settlers.game-test
  (:require [clojure.test :refer :all]
            [settlers.create :as create]
            [settlers.game :refer :all]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def game (create/-create-game terrains))

(deftest t-basic
  (let [g (-> game
              (create/add-player "Bryn"))]
    (is (= (tiles-for-vertex g [[0 0] :n])
           '({:terrain :forest, :roll 11} {:terrain :forest, :roll 10} {:terrain :hill, :roll 9})))

    (is (= (tiles-for-vertex g [[-2 3] :w])
           '({:terrain :field, :roll 12})))

    (is (= (tiles-for-vertex g [[2 -2] :w])
           '({:terrain :mountain, :roll 6} {:terrain :pasture, :roll 2})))
    ))


(def g (-> game
           (create/add-player "Bryn")
           (create/add-player "Mark")
           (create/add-settlement :bryn [[0 0] :n])
           (create/add-settlement :bryn [[-2 2] :n])
           (create/add-settlement :mark [[1 1] :n])
           (create/add-settlement :mark [[-1 0] :w])))
