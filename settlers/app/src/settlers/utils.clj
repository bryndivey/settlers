(ns ^:shared settlers.utils)

(defn has-at-least?
  ([game player r-map]
     (has-at-least? (-> game :players player) r-map))
  ([player r-map]
     (let [r2 (merge-with - (:resources player) (select-keys r-map (keys (:resources player))))]
       r2
       (not-any? neg? (vals r2)))))

(defn- munge-resources
  "Modify the player's resources dict with an op, only modifying resources that exist in the player's map"
  ([op player r-map]
     (assoc player :resources
            (merge-with op
                        (:resources player)
                        (select-keys r-map (keys (:resources player))))))
  ([op game player r-map]
     (let [rpath [:players player :resources]
           resources (get-in game rpath)]
       (assoc-in game rpath
                 (merge-with op
                             resources
                             (select-keys r-map (keys resources)))))))

(defn dec-resources
  ([player r-map]
     (munge-resources - player r-map))
  ([game player-name r-map]
     (munge-resources - game player-name r-map)))

(defn inc-resources
  ([player r-map]
     (munge-resources + player r-map))
  ([game player r-map]
     (munge-resources + game player r-map)))

(defn g-p [game move] ((:players game) (:player move)))

(defn rotate-while 
  "https://groups.google.com/forum/#!topic/clojure/SjmevTjZPcQ
   Rotates a collection left while (pred item) is true. Will return a unrotated 
   sequence if (pred item) is never true. Executes in O(n) time." 
  [pred coll] 
  (let [head (drop-while pred coll)] 
    (take (count coll) (concat head coll)))) 

(defn rotate [n s] 
  (let [[front back] (split-at (mod n (count s)) s)] 
    (concat back front))) 
