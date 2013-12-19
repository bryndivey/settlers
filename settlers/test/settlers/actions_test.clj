(ns settlers.actions-test
  (:require [clojure.test :refer :all]
            [settlers.utils :refer :all]
            [settlers.create :as create]
            [settlers.game :refer :all]
            [settlers.actions :refer :all]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def cards [{:type :road-building} {:type :knight} {:type :victory-point} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :monopoly} {:type :victory-point} {:type :knight} {:type :year-of-plenty} {:type :victory-point} {:type :knight} {:type :victory-point} {:type :monopoly} {:type :knight} {:type :knight} {:type :road-building} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :victory-point} {:type :year-of-plenty} {:type :knight}])

(def g (-> (create/-create-game terrains cards)
           (create/add-player "Bryn")
           (create/add-settlement :bryn [[0 0] :n])
           (create/add-settlement :bryn [[-2 2] :n])
           (create/add-road :bryn #{[-1 0] [0 0]})
           (inc-resources :bryn {:wood 1 :wool 1 :brick 1 :grain 1 :ore 1})))

(deftest t-v-settlement
  (is (= false (v-settlement-location g nil {:target [[0 1] :w]})))  
  (is (= false (v-settlement-location g nil {:target [[0 0] :w]})))
  (is (= true (v-settlement-location g nil {:target [[1 0] :w]})))
  )

(deftest t-v-road-location
  (is (= true (v-road-location g {:id :bryn} {:target #{[-1 1] [0 0]}}))))

(deftest t-build-road
  (is (= (-> (perform-move g {:player :bryn
                              :action :build-road
                              :target #{[-1 1] [0 0]}})
             :edges
             (get #{[-1 1] [0 0]}))
         {:type :road, :position #{[0 0] [-1 1]}, :player :bryn})))


(deftest t-deal-card
  (let [g' (perform-move g {:player :bryn :action :buy-development-card})]
    (is (= (get-in g' [:players :bryn :cards] [{:type :road-building}])))
    (is (= (:cards g') (rest cards)))))
