(ns settlers.game-test
  (:require [clojure.test :refer :all]
            [settlers.utils :refer [inc-resources]]
            [settlers.create :as create]
            [settlers.game :refer :all]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def cards [{:type :road-building} {:type :knight} {:type :victory-point} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :monopoly} {:type :victory-point} {:type :knight} {:type :year-of-plenty} {:type :victory-point} {:type :knight} {:type :victory-point} {:type :monopoly} {:type :knight} {:type :knight} {:type :road-building} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :victory-point} {:type :year-of-plenty} {:type :knight}])

(def game (create/-create-game terrains cards))

(deftest t-basic
  (let [g (-> game
              (create/add-player "Bryn"))]
    (is (= (tiles-for-vertex g [[0 0] :n])
           '({:terrain :forest, :roll 11 :position [0 0]}
             {:terrain :forest, :roll 10 :position [0 -1]}
             {:terrain :hill, :roll 9 :position [1 -1]})))

    (is (= (tiles-for-vertex g [[-2 3] :w])
           '({:terrain :field, :roll 12 :position [-2 2]})))

    (is (= (tiles-for-vertex g [[2 -2] :w])
           '({:terrain :mountain, :roll 6 :position [2 -2]}
             {:terrain :pasture, :roll 2 :position [1 -2]})))
    ))


(def g (-> game
           (create/add-player "Bryn")
           (create/add-player "Mark")
           
           (create/add-settlement :bryn [[0 0] :n])
           (create/add-road :bryn [[0 0] [-1 0]])
           (create/add-settlement :bryn [[2 -2] :n])
           (create/add-road :bryn [[1 -2] [1 -1]])
           
           (create/add-settlement :mark [[1 1] :n])
           (create/add-road :mark [[1 1] [0 1]])
           (create/add-settlement :mark [[-1 0] :w])
           (create/add-road :bryn [[-1 0] [-2 0]])))

(deftest t-resources
  (is (= '({:brick 1, :wood 0, :ore 0, :wool 0, :grain 1}
           {:brick 0, :wood 0, :ore 0, :wool 0, :grain 0})
         (map :resources (vals (:players (do-resource-allocation g 8)))))))

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
