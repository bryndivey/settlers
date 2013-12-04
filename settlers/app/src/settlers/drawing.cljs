(ns settlers.drawing
  (:require [domina :as dom]
            [domina.css :as css]
            [io.pedestal.app.util.log :as log]
            [settlers.canvas :as c]
            [settlers.create :as create]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def g (create/-create-game terrains))

(def hex-size 41)

(defn draw-tile [ctx {:keys [terrain roll position] :as tile}]
  (let [[q r] position
        s hex-size
        {:keys [dx dy w h]} (c/hex-geo s)]

    (c/fill-style ctx "#03c070")

    ;; apparently I did magic in here I don't fully understand
    (c/hex ctx
           (+ (* (+ 2 r (/ q 2)) w))
           (* (+ 2 q) (+ s dy))
           s)))

(defn draw-game [ctx g]
  (doseq [row (:map g)
          tile row
          :when tile]
    (draw-tile ctx tile)))

(defn draw-board [ctx x y]
  (let [{:keys [dx dy w h]} (c/hex-geo hex-size)]
    (doseq [i (range 3)]
      (c/hex ctx (+ x (* i w) (* dx 2)) 0 hex-size))
    (doseq [i (range 4)]
      (c/hex ctx (+ x (* i w) dx) (+ y hex-size dy) hex-size))
    (doseq [i (range 5)]
      (c/hex ctx (+ x (* i w)) (+ y (* 2 (+ hex-size dy))) hex-size))
    (doseq [i (range 4)]
      (c/hex ctx (+ x (* i w) dx) (+ y (* 3 (+ hex-size dy))) hex-size))
    (doseq [i (range 3)]
      (c/hex ctx (+ x (* i w) (* dx 2)) (+ y (* 4 (+ hex-size dy))) hex-size))))

(defn initialize [node]
  (let [ctx (c/get-context (dom/by-id "canvas") :2d)]
    (draw-game ctx g)))


