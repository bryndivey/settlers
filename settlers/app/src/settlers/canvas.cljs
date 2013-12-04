(ns settlers.canvas)

(defn get-context [id w h]
  (js/Raphael. id w h))


(defn text [ctx x y text]
  (.text ctx x y text))

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

(defn hex-position [[q r] s]
  (let [{:keys [dx dy w h]} (hex-geo s)]
    {:x (+ (* (+ 2 r (/ q 2)) w))
     :y (* (+ 2 q) (+ s dy))}))

(defn hex-points [x y s]
  (let [{:keys [dx dy w h]} (hex-geo s)]
    {:n [(+ x dx) y]
     :ne [(+ x w) (+ y dy)]
     :se [(+ x w) (+ y dy s)]
     :s [(+ x dx) (+ y h)]
     :sw [x (+ y dy s)]
     :nw [x (+ y dy)]}))

(defn hex [ctx x y size]
  (let [{:keys [n ne se s sw nw]} (hex-points x y size)
        move-to (fn [x y] (format "M%d %d" x y))
        line-to (fn [x y] (format "L%d %d" x y))
        path (str (apply move-to n) (apply str (map #(apply line-to %) [ne se s sw nw n])))]
    (.path ctx path)))

(defn rect [ctx x y w h]
  (.rect ctx x y w h))

(defn circle [ctx x y r]
  (.circle ctx x y r))

(defn set [ctx & rest]
  (let [s (.set ctx)]
    (doseq [o rest]
      (.push s o))
    s))

(defn set-onclick! [o f]
  (.click o f)
  o)

(defn transform! [o s]
  (.transform o s)
  o)
