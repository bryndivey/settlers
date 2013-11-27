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

(defn- norm-pos [g i]
  "Normalize offset to absolute position"
  (+ (/ (- (count g) 1) 2) i))

(defn get-q [g q]
  "Get a q face slice from a grid"
  (get g (norm-pos g q)))

(defn get-r [g r]
  "Get an r face slice from a grid"
  (map #(get % (norm-pos g r))))

(defn get-qr [g [q r]]
  "Get a qr face"
  (get-q (get-q g q) r))

(defn face-neighbours [[q r]]
  (for [[q' r'] [[1 0] [1 -1] [0 -1] [-1 0] [-1 1] [0 1]]]
    [(+ q q') (+ r r')]))

(defn get-neighbours 
  "Get the neighbouring faces of a [q r] face"
  [g p]
  (filter identity (map (partial get-qr g) (face-neighbours p))))

(defn distance [[q1 r1] [q2 r2]]
  "Distance between two faces"
  (let [abs #(java.lang.Math/abs %)]
    (/ (+ (abs (- q1 q2))
          (abs (- r1 r2))
          (abs (- (+ q1 r1) q2 r2)))
       2)))

(defn set-qr [g [q r] v]
  (let [i (map (partial norm-pos g) [q r])]
    (assoc-in g i v)))


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

