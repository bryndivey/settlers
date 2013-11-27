(ns settlers.map-test
  (:require [settlers.map :refer :all]
            [clojure.test :refer :all]))



(def test-map-1
  [[nil :pasture :hill]
   [:hill :field :mountain]
   [:forest :hill nil]])

(deftest test-map-funcs
  ;; q rows
  (is (= [nil :pasture :hill] (get-q test-map-1 -1)))
  (is (= [:hill :field :mountain] (get-q test-map-1 0)))
  (is (= [:forest :hill nil] (get-q test-map-1 1)))

  ;; r rows
  (is (= [nil :hill :forest] (get-r test-map-1 -1)))
  (is (= [:pasture :field :hill] (get-r test-map-1 0)))
  (is (= [:hill :mountain nil] (get-r test-map-1 1)))

  ;; qr faces
  (is (= :field (get-qr test-map-1 [0 0])))
  (is (= nil (get-qr test-map-1 [10 10])))
  (is (= :mountain (get-qr test-map-1 [0 1])))

  ;; neighbours
  (is (= (set [:pasture :hill :forest :hill :mountain :hill])
         (set (get-neighbours test-map-1 [0 0]))))
  (is (= (set [:hill :field :hill])
         (set (get-neighbours test-map-1 [-1 0]))))
  (is (= (set [:hill :mountain])
         (set (get-neighbours test-map-1 [1 1]))))  
  
  )





