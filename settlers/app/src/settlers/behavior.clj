(ns ^:shared settlers.behavior
    (:require [clojure.string :as string]
              [io.pedestal.app :as app]
              [io.pedestal.app.messages :as msg]))

;; TRANSFORMS

(defn swap-transform [_ message]
  (:value message))

(defn move-transform [t message]
  (assoc (select-keys message [:player :slot :targets]) :nub (rand)))

(defn inc-transform [old _]
  ((fnil inc 1) old))


;; INITIALIZERS

(defn init-game [_]
  [[:transform-enable [:main :move]
    :perform-move [{msg/topic [:move]
                    (msg/param :player) {:read-as :data}
                    (msg/param :action) {:read-as :data}
                    (msg/param :target) {:read-as :data}}]]])

(defn send-move [inputs]
  (let [message (:message inputs)
        game (get-in inputs [:new-model :game])]
    [{msg/type :perform-move :msg/topic [:game] :game game :move (select-keys
                                                                  message
                                                                  [:action :player :target])}]))


(def example-app
  {:version 2
   :transform [[:swap [:**] swap-transform]
               [:perform-move [:move] move-transform]]

   :effect #{{:in #{[:move]} :fn send-move}}

   :emit [{:init init-game}
          [#{[:move]
             [:error]
             [:tick]
             
             [:game :map]
             [:game :edges]
             [:game :vertices]
             [:game :next-move]
             [:game :moves]
             [:game :last-roll]

             [:game :players :* :name]
             [:game :players :* :resources]
             [:game :players :* :moves]
             [:game :players :* :hand]
             } (app/default-emitter [:main])]
          ]   
   })

