(ns settlers.actions-test
  (:require [clojure.test :refer :all]
            [settlers.create :as create]
            [settlers.game :refer :all]
            [settlers.actions :refer :all]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def g (-> (create/-create-game terrains)
           (create/add-player "Bryn")
           (create/add-settlement :bryn [[0 0] :n])
           (create/add-settlement :bryn [[-2 2] :n])))

(deftest t-v-settlement
  (is (= true (v-settlement-location g [[0 0] :n])))
  (is (= false (v-settlement-location g [[-1 -1] :n]))))
