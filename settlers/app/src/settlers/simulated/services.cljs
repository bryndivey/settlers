(ns settlers.simulated.services
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.util.log :as log]
            [settlers.actions :as actions]
            [settlers.game :as game]))

(defn new-game [input-queue g]
  (p/put-message input-queue {msg/type :swap
                              msg/topic [:game]
                              :value g}))

(defn perform-move [input-queue g m]
  (let [valid (actions/valid-move? g m)]
    (if valid
      (let [n (game/game-loop g m)]
        (new-game input-queue n))
      (p/put-message input-queue {msg/type :swap
                                  msg/topic [:error]
                                  :value "INVALID MOVE!"}))))

(defn tick [input-queue g]
  (let [n (game/game-tick g)]
    (new-game input-queue n)))

(defn services-fn [message input-queue]
  (let [type (msg/type message)]
    (cond
     (= type :perform-move) (perform-move input-queue
                                          (:game message)
                                          (:move message))
     :else (log/error "Unhandled message of type" type))))
