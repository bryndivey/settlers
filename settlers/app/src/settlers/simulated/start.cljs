(ns settlers.simulated.start
  (:require [io.pedestal.app :as app]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [settlers.start :as start]
            [settlers.simulated.services :as services]
            [settlers.rendering :as rendering]
            [goog.Uri]
            ;; This needs to be included somewhere in order for the
            ;; tools to work.
            [io.pedestal.app-tools.tooling :as tooling]))

(defn param [name]
  (let [uri (goog.Uri. (.toString  (.-location js/document)))]
    (.getParameterValue uri name)))

(defn ^:export main []
  (let [app (start/create-app (if (= "auto" (param "renderer"))
                                d/data-renderer-config
                                (rendering/render-config)))]
    (app/consume-effects (:app app) services/services-fn)
    app))
