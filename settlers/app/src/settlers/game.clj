(ns ^:shared settlers.game
    (:require [settlers.map :as map]))

(defn tiles-for-vertex [g v]
  ;; return the tiles surrounding a vertex - eg, for initial resource allocation
  (let [faces (map/vertex-to-faces v)]
    (map #(map/get-qr (:map g) %) faces)))
