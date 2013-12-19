(ns settlers.actions-test
  (:require [clojure.test :refer :all]
            [settlers.utils :refer :all]
            [settlers.create :as create]
            [settlers.game :refer :all]
            [settlers.actions :refer :all]
            [settlers.test-vals :as vals]))

(def game vals/game)

(deftest t-v-settlement
;  (is (= false (v-settlement-location game {:id :bryn} {:target [[0 1] :w]})))  
;  (is (= false (v-settlement-location game {:id :bryn} {:target [[0 0] :w]})))
  (is (= true (v-settlement-location game {:id :bryn} {:target [[1 -1] :n] :player :bryn})))
  )

(deftest t-v-road-location
  (is (= true (v-road-location game
                               {:id :bryn}
                               {:target #{[-1 1] [0 0]} :player :bryn}))))

(deftest t-build-road
  (is (= (-> game
             (inc-resources :bryn {:wood 1 :brick 1})
             (perform-move  {:player :bryn
                             :action :build-road
                             :target #{[-1 1] [0 0]}})
             :edges
             (get #{[-1 1] [0 0]}))
         {:type :road, :position #{[0 0] [-1 1]}, :player :bryn})))


(deftest t-deal-card
  (let [g' (-> game
               (inc-resources :bryn {:wool 1 :ore 1 :grain 1})
               (perform-move {:player :bryn :action :buy-development-card}))]
    (is (= (get-in g' [:players :bryn :cards] [{:type :road-building}])))
    (is (= (:cards g') (rest vals/cards)))))
