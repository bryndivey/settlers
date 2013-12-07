(ns settlers.rendering
  (:require [domina :as dom]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.render.events :as events]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [io.pedestal.app.util.log :as log]
            [settlers.drawing :as drawing])
  (:require-macros [settlers.html-templates :as html-templates]))

;; Load templates.

(def templates (html-templates/settlers-templates))

(defn render-game [renderer [_ path] transmitter]
  (let [path (vector (first path))
        parent (render/get-parent-id renderer path)
        id (render/new-id! renderer path)
        html (templates/add-template renderer path (:settlers-page templates))
        node (html {:id id :message ""})]
    (.log js/console "INITIAL")
    (dom/append! (dom/by-id parent) node)
    (drawing/initialize)))


(defn make-move-fn [game transmitter]
  (fn [move]
    (let [msg (assoc move
                msg/type :perform-move
                msg/topic [:move]
                :player (-> game
                            :next-move
                            :player))]
      (.log js/console "SENDING" (str msg))
      (events/send-transforms transmitter :perform-move [msg]))))

(defn update-game [renderer [_ path _ game] transmitter]
  (drawing/render game (make-move-fn game transmitter)))

(defn update-error [renderer [_ path _ new-value] transmitter]
  (dom/append! (dom/by-id "error") (dom/html-to-dom (str new-value))))

(defn render-config []
  [
   [:node-create  [:main :game] render-game]
   [:node-destroy   [:game] d/default-exit]

   [:value [:main :game] update-game]
   [:value [:main :error] update-error]]
  )

