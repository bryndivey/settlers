(ns settlers.actions-test
  (:require [clojure.test :refer :all]
            [settlers.utils :refer :all]
            [settlers.create :as create]
            [settlers.game :refer :all]
            [settlers.actions :refer :all]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def g (-> (create/-create-game terrains)
           (create/add-player "Bryn")
           (create/add-settlement :bryn [[0 0] :n])
           (create/add-settlement :bryn [[-2 2] :n])
           (create/add-road :bryn #{[-1 0] [0 0]})
           (inc-resources :bryn {:wood 1 :wool 1 :brick 1 :grain 1})))

(deftest t-v-settlement
;  (is (= true (v-settlement-location g nil {:target [[0 1] :w]})))  
  (is (= false (v-settlement-location g nil {:target [[0 0] :w]})))
;  (is (= false (v-settlement-location g nil {:target [[1 0] :w]})))
  )


