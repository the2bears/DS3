(ns ds3.fps
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [ds3.common :as c]))

(defscreen fps-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (stage)
             :camera (orthographic :set-to-ortho false))
    (assoc (label "0" (color :white) :set-font-scale 1.0 1.0)
      :id :fps
      :x (c/screen-to-world 5)
      )
    )

  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity))
         (render! screen))))
