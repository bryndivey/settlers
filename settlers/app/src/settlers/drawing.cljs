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

(defn hex-position [[q r]]
  (let [{:keys dx dy w h} (c/hex-geo hex-size)]
    {:x (+ (* (+ 2 r (/ q 2)) w))
     :y (* (+ 2 q) (+ s dy))}))

(defn draw-tile [ctx {:keys [terrain roll position] :as tile}]
  "Creates a new canvas element for this tile."
  (let [s hex-size
        {:keys [dx dy w h]} (c/hex-geo s)
        {:keys [x y]} (hex-position position)

        hex (c/set-fill! (c/hex ctx x y s) (terrain-colors terrain))
        text (.attr (c/centered-text ctx (+ x (/ w 2)) (+ y (/ h 2)) (str roll)) "font-size" 25)
        tile (c/set ctx hex text)]

    (c/set-onclick! tile (fn [e] (.log js/console "CLICKED!" (str position))))))


(defn draw-game [n g]
  (let [ctx (c/get-context "canvas" 1000 600)
        background (c/set-fill! (c/rect ctx 0 0 1000 600) "#77f")]
    (doseq [row (:map g)
            tile row
            :when tile]
      (draw-tile ctx tile))))



(defn initialize [node]
  (let [n (dom/by-id "canvas")]
    (draw-game n g)))


