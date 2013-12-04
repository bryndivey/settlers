(ns settlers.canvas)

(defn get-context [id w h]
  (js/Raphael. id w h))


(defn text [ctx x y text]
  (. ctx text x y text))

(defn centered-text [ctx x y t]
  "x and y define the midpoint of the text, not the top-left"
  (text ctx x y t))



(defn set-fill! [o fill]
  (.attr o "fill" fill))

(defn to-radians [d]
  (* d (/ Math/PI 180)))

(defn hex-geo [s]
  (let [r (to-radians 30)
        dy (Math/floor (Math/abs (* (Math/sin r) s)))
        dx (Math/floor (Math/abs (* (Math/cos r) s)))]
    {:dy dy :dx dx :w (* 2 dx) :h (+ (* 2 dy) s)}))

(defn hex [ctx x y s]
  (let [{:keys [dx dy w h]} (hex-geo s)
        move-to (fn [x y] (format "M%d %d" x y))
        line-to (fn [x y] (format "L%d %d" x y))]
    (. ctx path (str (move-to (+ x dx) y)
                     (line-to (+ x w) (+ y dy))
                     (line-to (+ x w) (+ y dy s))
                     (line-to (+ x dx) (+ y h))
                     (line-to x (+ y dy s))
                     (line-to x (+ y dy))
                     (line-to (+ x dx) y)))))

(defn rect [ctx x y w h]
  (. ctx rect x y w h))
