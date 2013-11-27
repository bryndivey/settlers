(ns ^:shared settlers.map)

(defn a2c [[q r]]
  "Axial co-ords to cube co-ords"
  (let [x q
        z r
        y (- (+ x z))]
    [x y z]))

(defn c2a [[x y z]]
  "Cube co-ords to axial co-ords"
  [x z])

(defn get-q [g q]
  "Get a q face slice from a grid"
  (let [m (/ 2 (- (count g) 1))]
    (get g (+ m q))))

(defn get-r [g r]
  "Get an r face slice from a grid"
  (let [m (/ 2 (- (count g) 1))]
    (map #(get % (+ m r)) g)))

(defn get-qr [g [q r]]
  "Get a qr face"
  (get-q (get-q g q) r))

(defn get-neighbours 
  "Get the neighbouring faces of a [q r] face"
  ([[q r]]
     (for [[q' r'] [[1 0] [1 -1] [0 -1] [-1 0] [-1 1] [0 1]]]
       [(+ q q') (+ r r')]))
  ([g p]
     (filter identity (map (partial get-qr g) (get-neighbours p)))))
