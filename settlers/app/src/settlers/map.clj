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

(defn- norm-pos [m i]
  "Normalize offset to absolute position"
  (+ (/ (- (count m) 1) 2) i))

(defn get-q [m q]
  "Get a q face slice from a grid"
  (get m (norm-pos m q)))

(defn get-r [m r]
  "Get an r face slice from a grid"
  (map #(get % (norm-pos m r)) m))

(defn get-qr [m [q r]]
  "Get a qr face"
  (get-q (get-q m q) r))

(defn face-neighbours [[q r]]
  (for [[q' r'] [[1 0] [1 -1] [0 -1] [-1 0] [-1 1] [0 1]]]
    [(+ q q') (+ r r')]))

(defn get-neighbours 
  "Get the neighbouring faces of a [q r] face"
  [m p]
  (filter identity (map (partial get-qr m) (face-neighbours p))))

(defn distance [[q1 r1] [q2 r2]]
  "Distance between two faces"
  (let [abs #(java.lang.Math/abs %)]
    (/ (+ (abs (- q1 q2))
          (abs (- r1 r2))
          (abs (- (+ q1 r1) q2 r2)))
       2)))

(defn set-qr [m [q r] v]
  (let [i (map (partial norm-pos m) [q r])]
    (assoc-in m i v)))


;; vertices
;; face + n/w 

(defn vertex-neighbours [[[q r] d]]
  (cond (= d :n) [[[q r] :w] [[(+ q 1) r] :w] [[(+ q 1) (- r 1)] :w]]
        (= d :w) [[[q r] :n] [[(- q 1) r] :n] [[(- q 1) (+ r 1)] :n]]
        :else (throw (Exception. (str "Invalid direction" d)))))

(defn vertex-to-faces [[[q r] d]]
  (cond (= d :n) [[q r] [q (- r 1)] [(+ q 1) (- r 1)]]
        (= d :w) [[q r] [(- q 1) r] [q (- r 1)]]
        :else (throw (Exception. (str "Invalid direction" d)))))

;; edges
;; edges are two faces, which are a q and r each

(defn e-dir [[[q1 r1] [q2 r2]]]
  "get the 'direction' of a face"
  (cond
   (= r1 r2) :x
   (= q1 q2) :y
   (= (+ q1 r1) (+ q2 r2)) :z))

(defn e-opposite-tiles [[[q1 r1] [q2 r2]]]
  "get the 'opposite' tiles to find neighbouring edges"
  (case (e-dir [[q1 r1] [q2 r2]])
    :x [[q1 (+ r1 1)] [q2 (- r2 1)]]
    :y [[(- q1 1) (+ r1 1)] [(+ q2 1) (- r2 1)]]
    :z [[q1 (- r1 1)] [q2 (+ r2 1)]]))
