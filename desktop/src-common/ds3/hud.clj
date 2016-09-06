(ns ds3.hud
  (:require [play-clj.core :refer [color defscreen game orthographic render! screen! stage update!]]
            [play-clj.g2d :refer [bitmap-font]]
            [play-clj.ui :refer [label label! style]]
            [ds3.common :as c]))

(def y-padding 4.0)

(defscreen hud-screen
  :on-show
  (fn [screen entities]
    (let [p1-1up (label "1UP" (style :label (bitmap-font "arcade20.fnt") (color :yellow)))
          p1-1up-x (- (/ (game :width) 4) (label! p1-1up :get-pref-width))
          p1-1up-y (+ y-padding (label! p1-1up :get-pref-height))
          p1-score (label "0" (style :label (bitmap-font "arcade20.fnt") (color :white)))
          p1-score-x (- (/ (game :width) 4) (- (label! p1-score :get-pref-width) 20))
          p1-score-y (* p1-1up-y 2)]
      (update! screen
               :renderer (stage)
               :camera (orthographic :set-to-ortho false)
               :p1-score 0)
      (prn :hud :height (game :height) :width (game :width))
      (prn :ps-1up :height (label! p1-1up :get-pref-height) :width (label! p1-1up :get-pref-width))
      [(assoc p1-1up :id :p1-1up :x p1-1up-x :y (- (game :height) p1-1up-y))
       (assoc p1-score :id :p1-score :x p1-score-x :y (- (game :height) p1-score-y))]
      ))


  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :p1-score (do
                         (doto entity (label! :set-text (str (:p1-score screen))))
                         (let [p1-score-x (- (/ (game :width) 4) (- (label! entity :get-pref-width) 20))]
                           (assoc entity :x p1-score-x)))
             entity))
         (render! screen)))


  ;Called by the main_screen, passing in :score
  :on-update-score
  (fn [screen entities]
    (let [score (:p1-score screen)]
      (update! screen :p1-score score)
      ;(prn :score score)
      )
    nil)

  :on-reset-score
  (fn [screen entities]
    (update! screen :p1-score 0)
    nil))
