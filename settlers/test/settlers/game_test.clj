(ns settlers.game-test
  (:require [clojure.test :refer :all]
            [settlers.utils :refer [inc-resources]]
            [settlers.create :as create]
            [settlers.game :refer :all]
            [settlers.test-vals :as vals]))

(def g vals/game)

(deftest t-basic
  (is (= (tiles-for-vertex g [[0 0] :n])
         '({:terrain :forest, :roll 11, :position [0 0]}
           {:terrain :hill, :roll 3, :position [-1 0]}
           {:terrain :pasture, :roll 6, :position [-1 1]})))

  (is (= (tiles-for-vertex g [[-2 3] :w])
         '({:terrain :field, :roll 12 :position [-2 2]})))

  (is (= (tiles-for-vertex g [[2 -2] :w])
         '({:terrain :mountain, :roll 6 :position [2 -2]}
           {:terrain :pasture, :roll 2 :position [1 -2]})))
  )


(deftest t-resources
  (is (= '({:brick 1, :wood 0, :ore 0, :wool 0, :grain 1}
           {:brick 0, :wood 0, :ore 0, :wool 0, :grain 0})
         (map :resources (vals (:players (do-resource-allocation g 8))))))
  (is (= '({:brick 0, :wood 1, :ore 0, :wool 0, :grain 0}
           {:brick 0, :wood 0, :ore 0, :wool 0, :grain 0})
         (map :resources (vals (:players (do-resource-allocation g 5))))))  )

(deftest t-next-player
  (is (= (next-player g :bryn) :mark))
  (is (= (next-player g :mark) :bryn)))

(deftest t-game-loop
  (is (= (:next-move g) {:player :mark :type :game-action}))
  (let [g' (game-loop g {:player :mark :action :end-turn})]
    (is (= (:next-move g'
            {:player :bryn :type :game-action}))))
  (let [g' (-> g
               (inc-resources :mark {:wood 1 :brick 1})
               (game-loop {:player :mark :action :build-road :target #{[1 0] [1 1]}}))]
    (is (= (-> g' :edges (get  #{[1 0] [1 1]}))
           {:type :road, :position #{[1 0] [1 1]}, :player :mark})))
  (let [g' (-> g
               (game-loop {:player :mark :action :build-road :target #{[1 0] [1 1]}}))]
    (is (= (:next-move g') {:player :mark :type :game-action}))
    (println (keys g'))
    (is (= (:error g') "Invalid move"))))
