(ns ds3.hud
  (:require [play-clj.core :refer [color defscreen game orthographic render! stage update!]]
            [play-clj.ui :refer [label label!]]
            [ds3.common :as c]))

(defscreen hud-screen
  :on-show
  (fn [screen entities]
    (update! screen
             :renderer (stage)
             :camera (orthographic :set-to-ortho false)
             :score 0)

    (assoc (label "0" (color :red) :set-font-scale 5.0 5.0)
      :id :score
      :x c/game-width :y 20
      )
    )
  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :score (doto entity (label! :set-text (str (:score screen))))
             entity))
         (render! screen)))


  ;Called by the main_screen, passing in :score
  :on-update-score
  (fn [screen entities]
    (let [score (:score screen)]
      (update! screen :score score)
      ;(prn :score score)
      )
    nil)

  :on-reset-score
  (fn [screen entities]
    (update! screen :score 0)
    nil))
