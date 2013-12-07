(ns settlers.canvas)

(defn get-context [id w h]
  (js/Raphael. id w h))


(defn text [ctx x y text]
  (let [t (.text ctx x y text)]
    (.attr t"text-anchor" "start")
    t))

(defn centered-text [ctx x y t]
  "x and y define the midpoint of the text, not the top-left"
  (.text ctx x y t))



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

(defn move-to! [o x y]
  (transform! o (format "T%d,%d" x y)))

(defn move! [o x y]
  (transform! o (format "t%d,%d" x y)))

(defn rotate! [o d]
  (transform! o (format "R%d" d)))

(defn move-to-origin! [o]
  "Moves a centered thing to origin"
  (let [bbox (.getBBox o)]
    ; TODO - y
    (transform! o (format "t%d,%d" (/  (.-width bbox) 2) 0))
    o))

(defn remove! [o]
  (.remove o))
