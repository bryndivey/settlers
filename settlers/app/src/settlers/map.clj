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

(defn face-dir-neighbours [f]
  "Neighbours in cardinal directions"
  (into {} (map vector [:se :sw :w :nw :ne :e] (face-neighbours f))))

(defn get-neighbours 
  "Get the neighbouring faces of a [q r] face"
  [m p]
  (filter identity (map (partial get-qr m) (face-neighbours p))))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn distance [[q1 r1] [q2 r2]]
  "Distance between two faces"
  (/ (+ (abs (- q1 q2))
        (abs (- r1 r2))
        (abs (- (+ q1 r1) q2 r2)))
     2))

(defn adjacent? [fs]
  (every? #(<= % 1) (for [f1 fs
                          f2 fs] (distance f1 f2))))

(defn set-qr [m [q r] v]
  (let [i (map (partial norm-pos m) [q r])]
    (assoc-in m i v)))

(defn faces-expand [fs]
  "Expand given faces by one neighbouring set (for the sea edges)"
  (set (reduce #(into %1 (face-neighbours %2)) fs fs)))

(defn order-faces [fs]
  "Order faces into vector on q ordering"
  (sort-by first (sort-by second (vec fs))))

;; vertices
;; face + n/w 

(defn vertex-neighbours [[[q r] d]]
  (cond (= d :n) [[[q r] :w] [[(+ q 1) r] :w] [[(+ q 1) (- r 1)] :w]]
        (= d :w) [[[q r] :n] [[(- q 1) r] :n] [[(- q 1) (+ r 1)] :n]]
        :else (throw (Exception. (str "Invalid direction" d)))))

(defn vertex-to-faces [[[q r] d]]
  (cond (= d :n) [[q r] [(- q 1) r] [(- q 1) (+ r 1)]]
        (= d :w) [[q r] [(- q 1) r] [q (- r 1)]]
        :else (throw (Exception. (str "Invalid direction" d)))))


(defn v3-to-v1 [fs]
  "Convert a 3-face vertices to a 1-face, 1-dir vertex"
  (let [ordered (order-faces fs)
        ; are there more above or below
        weight (vals (frequencies (map first ordered)))]
    [(last ordered) (case weight
                      [1 2] :w
                      [2 1] :n)]))

(defn v1-to-v3 [[f d]]
  "Convert a 1-face 1-dir vertex to 3 faces"
  (let [neighbours (face-dir-neighbours f)]
    (case d
      :n [(:nw neighbours) (:ne neighbours) f]
      :w [(:w neighbours) (:nw neighbours) f])))


(defn all-vertices [fs]
  "All possible vertices for input faces"
  (let [fs' (set fs)
        all-fs (reduce #(into %1 (map (face-dir-neighbours %2) [:e :se :sw]))
                       fs' fs')
        all-fs' (filter #(or (fs' (:nw (face-dir-neighbours %)))
                             (fs' %)) all-fs)
        n-fs (filter #(fs' (:ne (face-dir-neighbours %))) all-fs)
        w-fs (filter #(fs' (:w (face-dir-neighbours %))) all-fs)

        both-vertices (reduce #(into %1 [[%2 :n] [%2 :w]]) #{} all-fs')
        plus-north (reduce #(into %1 [[%2 :n]]) both-vertices n-fs)
        plus-west (reduce #(into %1 [[%2 :w]]) plus-north w-fs)
        ]
    plus-west
))


;; edges
;; edges are two faces, which are a q and r each



(defn e-dir [e]
  "Get the 'direction' between two faces."
  (let [[[q1 r1] [q2 r2]] (order-faces e)]
    (cond
     (= r1 r2) :x
     (= q1 q2) :y
     (= (+ q1 r1) (+ q2 r2)) :z)))


(defn e-neighbours? [e]
    "Are two faces neighbours?"
    (let [[q1 r1] (first e)
          [q2 r2] (second e)
          s (Math/abs (+ (- q1 q2)
                         (- r1 r2)))]
      (<= s 1)))

(defn e-opposite-tiles [e]
  "get the 'opposite' tiles to find neighbouring edges"
  (let [[[q1 r1] [q2 r2]] (order-faces e)]
    (case (e-dir e)
      :x [[q1 (+ r1 1)] [q2 (- r2 1)]]
      :y [[(- q1 1) (+ r1 1)] [(+ q2 1) (- r2 1)]]
      :z [[q1 (- r1 1)] [q2 (+ r2 1)]])))

(defn e-neighbours [e]
  "Get neighbouring edges for an edge"
  (let [ordered (order-faces e)
        f1 (first ordered)
        f2 (second ordered)
        [f3 f4] (e-opposite-tiles [f1 f2])]
    #{#{f1 f3}
      #{f1 f4}
      #{f2 f3}
      #{f2 f4}}))

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


(defn e-to-v [e]
  "Convert an edge to a pair of vertices"
  (map #(conj e %) (e-opposite-tiles e))
  )


(defn next-edges [e gr]
  (gr e))

(def es #{#{[-1 0] [0 0]}
          #{[0 0] [0 -1]}
          #{[-1 0] [0 -1]}
          #{[0 -1] [1 -1]}
          #{[-1 -1] [0 -1]}
          #{[-1 0] [-1 -1]}
          #{[-2 0] [-1 0]}
          #{[-2 1] [-1 1]}
          #{[-1 0] [-1 1]}})

(def ess #{#{[-1 0] [0 0]}
           #{[-1 0] [0 -1]}
           #{[-1 0] [-1 -1]}
           #{[0 0] [0 -1]}
           #{[-2 0] [-1 0]}
           #{[-2 0] [-2 1]}
           #{[-2 1] [-1 0]}})



(defn- second-last-neighbours [p g]
  "Neighbours of the second-last element of path p"
  (if (> (count p) 1)
    (e-neighbours (nth p (- (count p) 2)))
    #{}))

(defn a' [ps g]
  "Taking paths ps and graph g, return new paths with ps extended with neighbours of the last path element, if that element isn't in the ps already"
  (for [p ps
        n (filter (e-neighbours (last p)) (g (last p)))
        :when (and (not ((set p) n))
                   (not ((second-last-neighbours p g) n)))]
    (conj p n)))


(defn longest-path
  "Find the longest path in graph gr"
  ([gr] (longest-path (set (map vector (keys gr))) gr))
  ([ps gr]
     (let [new (a' ps gr)
           ps' (into ps (a' ps gr))]
       (if (= ps ps')
         (apply max (map count ps'))
         (recur ps' gr)))))



