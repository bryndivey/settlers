(ns settlers.drawing
  (:require [domina :as dom]
            [domina.css :as css]
            [io.pedestal.app.util.log :as log]
            [settlers.map :as map]
            [settlers.canvas :as c]
            [settlers.create :as create]))

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(comment def g (-> (create/create-game)
           (create/add-player "Bryn")
           (create/add-player "Mark")
           (create/add-settlement :bryn [[0 0] :n])
           (create/add-settlement :bryn [[-2 2] :n])
           (create/add-settlement :mark [[1 1] :n])
           (create/add-settlement :mark [[-1 0] :w])
           (create/add-city :bryn [[1 -1] :w])

           (create/add-road :bryn [[0 0] [0 1]])
           (create/add-road :bryn [[0 0] [-1 1]])
           (create/add-road :bryn [[0 0] [-1 0]])
           (create/add-road :bryn [[0 0] [0 -1]])
           (create/add-road :bryn [[0 -1] [1 -1]])           
           ))

(def g (-> (create/create-game)
           (create/add-player "Bryn")
           (create/add-player "Mark")
           
           (create/add-settlement :bryn [[0 0] :n])
           (create/add-road :bryn [[0 0] [-1 0]])
           (create/add-settlement :bryn [[2 -2] :n])
           (create/add-road :bryn [[1 -2] [1 -1]])
           
           (create/add-settlement :mark [[1 1] :n])
           (create/add-road :mark [[1 1] [0 1]])
           (create/add-settlement :mark [[-1 0] :w])
           (create/add-road :mark [[-1 0] [-2 0]])))

(def hex-size 60)

(def terrain-colors
  {:desert "#DD8"
   :mountain "#777"
   :hill "#FB3"
   :field "#ff0"
   :forest "#181"
   :pasture "#FFF"}
  )

(defn player-color [player]
  ({:bryn "#F00"
    :mark "#00F"} player))


(defn draw-tile [ctx {:keys [terrain roll position] :as tile}]
  "Creates a new canvas element for this tile."
  (let [s hex-size
        {:keys [dx dy w h]} (c/hex-geo s)
        {:keys [x y]} (c/hex-position position hex-size)

        hex (c/set-fill! (c/hex ctx x y s) (terrain-colors terrain))
        text (.attr (c/centered-text ctx (+ x (/ w 2)) (+ y (/ h 2)) (str roll)) "font-size" 25)
        pos-text (.attr (c/centered-text ctx (+ x (/ w 2)) (+ y (/ h 2) 20) (str position)) "font-size" 10)
        tile (c/set ctx hex text)]

    (c/set-onclick! tile (fn [e] (.log js/console "CLICKED!" (str position))))))



(defn move-to-vertex [obj position]
  "Translates obj to vertex's x/y"
  (let [[pos d] position
        {:keys [x y]} (c/hex-position pos hex-size)
        points (c/hex-points x y hex-size)
        [x y] (case d
                :n (:n points)
                :w (:nw points))]
    (c/transform! obj (format "t%d,%d" x y))))

(defn color-for-player [obj player]
  (c/set-fill! obj (player-color player)))

(defn draw-settlement [ctx {:keys [position player]}]
  (-> (c/circle ctx 0 0 10)
      (move-to-vertex position)
      (color-for-player player)))

(defn draw-city [ctx {:keys [position player]}]
  (-> (c/rect ctx -10 -10 20 20)
      (move-to-vertex position)
      (color-for-player player)))

(defn draw-vertex-object [ctx obj]
  (case (:type obj)
    :settlement (draw-settlement ctx obj)
    :city (draw-city ctx obj)))



(defn draw-road [ctx obj]
  (let [[f1 f2] (:position obj)
        dir (map/e-dir (:position obj))
        road (c/rect ctx 0 0 8 hex-size)
        road (c/transform! road "R180")
        ordered (map/order-e [f1 f2])
        {:keys [x y]} (c/hex-position (first ordered) hex-size)
        points (c/hex-points x y hex-size)
        [angle pos off] (case dir
                          :x [-120 (:s points) "2,4"]
                          :y [180 (:se points) "4,0"]
                          :z [120 (:s points) "2,-3"])]
    (c/transform! road (format "R%d,0,0T%d,%dT%s" angle (first pos) (second pos) off))
    (color-for-player road (:player obj))

))

(defn draw-edge-object [ctx obj]
  (case (:type obj)
    :road (draw-road ctx obj)))

(defn draw-game [n g]
  (let [ctx (c/get-context "canvas" 1000 600)
        background (c/set-fill! (c/rect ctx 0 0 1000 600) "#77f")]
    ; tiles
    (doseq [row (:map g)
            tile row
            :when tile]
      (draw-tile ctx tile))

    ; edges
    (doseq [[_ obj] (:edges g)]
      (draw-edge-object ctx obj))

    ;vertices
    (doseq [[_ obj] (:vertices g)]
      (draw-vertex-object ctx obj))))



(defn initialize [node]
  (let [n (dom/by-id "canvas")]
    (draw-game n g)))


