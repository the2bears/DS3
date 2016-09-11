(ns ds3.fps
  (:require [play-clj.core :refer [color defscreen game orthographic render! stage update!]]
            [play-clj.g2d :refer [bitmap-font bitmap-font!]]
            [play-clj.ui :refer [label label! style]]
            [ds3.common :as c])
   (:import [com.badlogic.gdx.graphics.g2d Batch BitmapFont]))

(defscreen fps-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (stage)
             :camera (orthographic :set-to-ortho false)
             :font (bitmap-font "arcade20.fnt"))
    entities)

  :on-render
  (fn [screen entities]
    (let [renderer (:renderer screen)
          ^Batch batch (.getBatch renderer)
          arcade-fnt (:font screen)
          fps (game :fps)]
      (.begin batch)
      (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :white))
      (bitmap-font! ^BitmapFont arcade-fnt :draw batch (str fps) 5.0 24.0)
      (.end batch))
    (->> entities
         (render! screen))))
