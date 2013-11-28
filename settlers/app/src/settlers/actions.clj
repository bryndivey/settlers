(ns ^:shared settlers.actions
    (:require [settlers.map :refer [vertex-to-faces]]
              [settlers.create :refer []]))

(defn v-settlement-location [g v]
  "Is this vertex a valid place for a new settlement?
   Test - does it share two faces with an existing settlement"

  ; get all 3-face tuples for existing settlements
  ; check that the three faces for v share at most one with any of those
  (let [settlement-vertices (for [[v' s] (:vertices g)
                                  :when (#{:settlement :city} (:type s))]
                              v')
        used-faces (map vertex-to-faces settlement-vertices)
        v-faces (vertex-to-faces v)
        t (fn [fs]
            (> (count (clojure.set/intersection (set fs) (set v-faces)))
               2))]
    (boolean (some t used-faces))
))

