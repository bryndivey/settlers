(ns settlers.canvas)

(defn get-context [canvas type]
  (. canvas (getContext (name type))))

(defn fill-rect [ctx x y w h]
  (. ctx (fillRect x y w h))
  ctx)

(defn begin-path [ctx]
  (. ctx (beginPath))
  ctx)

(defn close-path [ctx x y]
  (. ctx (closePath))
  ctx)

(defn move-to [ctx x y]
  (. ctx (moveTo x y))
  ctx)

(defn line-to [ctx x y]
  (. ctx (lineTo x y))
  ctx)

(defn set-stroke-style [ctx color]
  (set! (.-strokeStyle ctx) color)
  ctx)

(defn set-stroke-width [ctx px]
  (set! (.-strokeWidth ctx) px)
  ctx)

(defn stroke [ctx]
  (. ctx (stroke))
  ctx)

(defn set-fill-style [ctx color]
  (set! (.-fillStyle ctx) color)
  ctx)

(defn fill [ctx]
  (. ctx (fill))
  ctx)

(defn set-font [ctx font]
  (set! (.-font ctx) font)
  ctx)

(defn text [ctx x y text]
  (. ctx (fillText text x y))
  ctx)

(defn centered-text [ctx x y t]
  "x and y define the midpoint of the text, not the top-left"
  (let [w (.-width (. ctx (measureText t)))
        baseline (.-textBaseline ctx)]
    (set! (.-textBaseline ctx) "middle")
    (text ctx (- x (/ w 2)) y t)
    (set! (.-textBaseline ctx) baseline)))





(defn circle [ctx x y r]
  (begin-path ctx)
  (. ctx (arc x y r 0 (* Math/PI 2) true))
  (close-path ctx)
  (stroke)
  ctx)

(defn to-radians [d]
  (* d (/ Math/PI 180)))

(defn hex-geo [s]
  (let [r (to-radians 30)
        dy (Math/floor (Math/abs (* (Math/sin r) s)))
        dx (Math/floor (Math/abs (* (Math/cos r) s)))]
    {:dy dy :dx dx :w (* 2 dx) :h (+ (* 2 dy) s)}))

(defn hex [ctx x y s]
  (let [{:keys [dx dy w h]} (hex-geo s)]
    (-> ctx
        (begin-path)
        (move-to (+ x dx) y)
        (line-to (+ x w) (+ y dy))
        (line-to (+ x w) (+ y dy s))
        (line-to (+ x dx) (+ y h))
        (line-to x (+ y dy s))
        (line-to x (+ y dy))
        (line-to (+ x dx) y)
        (close-path)
        (stroke)
        (fill)))
  ctx)
