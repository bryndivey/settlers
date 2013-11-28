(ns ^:shared settlers.actions
    (:require [settlers.utils :refer [has-at-least? dec-resources
                                      g-p]]
              [settlers.map :refer [vertex-to-faces]]
              [settlers.create :refer [add-settlement]]))

(def action-fns (atom {}))

(defn list-actions []
  (keys @action-fns))

(defn get-action-fn [action type]
  (get-in @action-fns [action type]))

(defn defaction [name & {:keys [validate-fns perform-fn]}]
  (assert perform-fn "Must define a perform-fn")
  (swap! action-fns assoc name {:validate validate-fns
                                :perform perform-fn}))


(defn validators-pass? [vfns game player args]
  (let [do-validate (fn [vfn]
                      (try
                        (vfn game player args)
                        (catch Exception e false)))]
    (or (not vfns)
        (every? true? (map do-validate vfns)))))


(defn valid-move? [game move]
  ; should fail-out on first error but i don't know the codez for that
  (assert (every? #{:player :action} (keys (select-keys move [:player :action]))) "Must have :player and :action on every move!")
  
  (let [player (g-p game move)
        vfns (get-action-fn (:action move) :validate)]
    (validators-pass? vfns game player (dissoc move :player :action))))

(defn perform-move [game move]
  (let [player (g-p game move)]

    (assert player "No such player!")
    (assert (valid-move? game move))

    (let [fn (get-action-fn (:action move) :perform)
          args (dissoc move :player :action)]
      (-> (fn game player args)
          (update-in [:moves] conj move)))))





;; actual moves

(defn v-settlement-location [g _ a]
  "Is this vertex a valid place for a new settlement?
   Test - does it share two faces with an existing settlement"

  ; get all 3-face tuples for existing settlements
  ; check that the three faces for v share at most one with any of those
  (let [v (:target a)
        settlement-vertices (for [[v' s] (:vertices g)
                                  :when (#{:settlement :city} (:type s))]
                              v')
        used-faces (map vertex-to-faces settlement-vertices)
        v-faces (vertex-to-faces v)
        t (fn [fs]
            (> (count (clojure.set/intersection (set fs) (set v-faces)))
               2))]
    (not-any? t used-faces)))

(defn v-building-number [type num]
  "Make a validator to ensure the player hasn't got too many buldings"
  (fn [g p a]
    true))

(defn v-required-resources [r-map]
  "Make a validator to ensure sufficient resources"
  (fn [g p _]
    (has-at-least? p r-map)))


(defaction :build-settlement
  :validate-fns [v-settlement-location
                 (v-required-resources {:wood 1 :wool 1 :brick 1 :grain 1})
                 (v-building-number :settlement 5)]
  :perform-fn (fn [g p a]
                (-> g
                    (add-settlement (:id p) (:target a))
                    (dec-resources (:id p) {:wood 1 :wool 1 :brick 1 :grain 1}))))

