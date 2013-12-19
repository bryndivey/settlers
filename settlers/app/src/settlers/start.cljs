(ns settlers.start
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app :as app]
            [io.pedestal.app.render.push :as push-render]
            [io.pedestal.app.render :as render]
            [io.pedestal.app.messages :as msg]
            [settlers.behavior :as behavior]
            [settlers.rendering :as rendering]
            [settlers.game :as game]
            [settlers.utils :as utils]))

;; In this namespace, the application is built and started.

(def terrains [:pasture :pasture :mountain :desert :pasture :hill :field :mountain :field :field :forest :field :forest :hill :mountain :forest :pasture :hill :forest])

(def cards [{:type :road-building} {:type :knight} {:type :victory-point} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :monopoly} {:type :victory-point} {:type :knight} {:type :year-of-plenty} {:type :victory-point} {:type :knight} {:type :victory-point} {:type :monopoly} {:type :knight} {:type :knight} {:type :road-building} {:type :knight} {:type :knight} {:type :knight} {:type :knight} {:type :victory-point} {:type :year-of-plenty} {:type :knight}])


(defn create-app [render-config]
  (let [app (app/build behavior/example-app)
        render-fn (push-render/renderer "content" render-config render/log-fn)
        app-model (render/consume-app-model app render-fn)
        r {:grain 10 :wood 10 :brick 10 :wool 10 :ore 10}]
    (app/begin app)
    (p/put-message (:input app)
                   {msg/type :swap
                    msg/topic [:game]
                    :value (-> (game/create terrains cards)
                               (utils/inc-resources :bryn r)
                               (utils/inc-resources :mark r))})
    {:app app :app-model app-model}))

(defn ^:export main []
  (create-app (rendering/render-config)))
