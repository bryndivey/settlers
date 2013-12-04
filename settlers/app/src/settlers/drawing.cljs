(ns settlers.drawing
  (:require [domina :as dom]
            [domina.css :as css]
            [io.pedestal.app.util.log :as log]
            [settlers.canvas :as c]
            [settlers.create :as create]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def g (create/-create-game terrains))

(def hex-size 60)

(def terrain-colors
  {:desert "#DD8"
   :mountain "#777"
   :hill "#FB3"
   :field "#ff0"
   :forest "#181"
   :pasture "#FFF"}
  )

(defn draw-tile [ctx {:keys [terrain roll position] :as tile}]
  (let [[q r] position
        s hex-size
        {:keys [dx dy w h]} (c/hex-geo s)
        x (+ (* (+ 2 r (/ q 2)) w))
        y (* (+ 2 q) (+ s dy))]

    ;; apparently I did magic in here I don't fully understand
    (c/set-fill-style ctx (terrain-colors terrain))
    (c/hex ctx x y s)
    
    (c/set-font ctx "25pt Helvetica bold")
    (c/set-fill-style ctx "#222")
    (c/centered-text ctx (+ x (/ w 2)) (+ y (/ h 2)) (str roll))))

(defn draw-game [ctx g]

  (c/set-fill-style ctx "#66e")
  (c/fill-rect ctx 0 0 1000 600)
  (doseq [row (:map g)
          tile row
          :when tile]
    (draw-tile ctx tile)))



(defn initialize [node]
  (let [ctx (c/get-context (dom/by-id "canvas") :2d)]
    (draw-game ctx g)))


