(ns settlers.test-vals
  (:require [settlers.game]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def cards [{:type :road-building} {:type :knight} {:type :victory-point} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :monopoly} {:type :victory-point} {:type :knight} {:type :year-of-plenty} {:type :victory-point} {:type :knight} {:type :victory-point} {:type :monopoly} {:type :knight} {:type :knight} {:type :road-building} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :victory-point} {:type :year-of-plenty} {:type :knight}])

(def game (settlers.game/create terrains cards))

