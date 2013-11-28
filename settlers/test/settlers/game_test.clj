(ns settlers.game-test
  (:require [clojure.test :refer :all]
            [settlers.create :as create]
            [settlers.game :refer :all]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def game (create/-create-game terrains))

(deftest t-basic
  (let [g (-> game
              (create/add-player "Bryn"))]
    (println g)))
