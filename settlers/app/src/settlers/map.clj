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

(defn order-e [e]
  "Order set e into vector on q ordering"
  (sort-by first (vec e)))

(defn e-dir [e]
  "Get the 'direction' between two faces."
  (let [[[q1 r1] [q2 r2]] (order-e e)]
    (cond
     (= r1 r2) :x
     (= q1 q2) :y
     (= (+ q1 r1) (+ q2 r2)) :z)))

(defn e-neighbours? [[f1 f2]]
  "Are two faces neighbours?"
  (let [[q1 r1] f1
        [q2 r2] f2
        s (Math/abs (+ (- q1 q2)
                       (- r1 r2)))]
    (<= s 1)))

(defn e-opposite-tiles [e]
  "get the 'opposite' tiles to find neighbouring edges"
  (let [[[q1 r1] [q2 r2]] (order-e e)]
    (case (e-dir e)
      :x [[q1 (+ r1 1)] [q2 (- r2 1)]]
      :y [[(- q1 1) (+ r1 1)] [(+ q2 1) (- r2 1)]]
      :z [[q1 (- r1 1)] [q2 (+ r2 1)]])))

(defn e-neighbours [e]
  "Get neighbouring edges for an edge"
  (let [f1 (first e)
        f2 (second e)
        [f3 f4] (e-opposite-tiles [f1 f2])]
    #{#{f1 f3}
      #{f1 f4}
      #{f2 f3}
      #{f2 f4}}))

(defn faces-expand [fs]
  "Expand given faces by one neighbouring set (for the sea edges"
  (set (reduce #(into %1 (face-neighbours %2)) fs fs)))

(defn e-valid [fs e]
  "Is this edge valid, given faces 'fs'? ie., are both faces either in m or direct neighbours of fs?"
  ; OPTIMIZE!
  (and (e-neighbours? e)
       (not= (first e) (second e))
       (every? (faces-expand fs) e)))

(defn face-edges [f]
  "All the edges for this face"
  (for [f' (face-neighbours f)]
    #{f f'}))

(defn faces-edges [fs]
  "All the edges between faces fs"
  (into #{} (apply concat (map face-edges fs))))

(defn e-graph [es]
  "Build a graph of connected edges for passed-in set of edges"
  (let [f (fn [g e]
            (assoc g e (filter es (e-neighbours e))))]
    (reduce f {} es)))

(defn e-face-graph [fs]
  "Build a graph of connected edges for passed-in faces"
  (e-graph (faces-edges fs)))


