(ns ds3.bullet
  (:require [play-clj.core :refer [color shape sound]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [ds3.common :as c]
            )
  (:import [com.badlogic.gdx.physics.box2d Filter]))

(def bullet-filter-group -1)

(def bullet-width (c/screen-to-world 2))
(def bullet-height (c/screen-to-world 2))
(def bullet-speed (c/screen-to-world 240));2400?
(def half-width (/ bullet-width 2))
(def half-height (/ bullet-height 2))

(defn modify-filter [fixture]
  (let [bullet-filter (fixture! fixture :get-filter-data)]
    (set! (.groupIndex bullet-filter) bullet-filter-group)
    (fixture! fixture :set-filter-data bullet-filter)))

(defn create-bullet-body!
  [screen x y]
  (let [body (add-body! screen (body-def :dynamic
                                         :bullet true))]
    (->> (polygon-shape :set-as-box half-width half-height (vector-2 half-width half-height) 0)
         (fixture-def :density 0 :friction 0 :restitution 0 :shape)
         (body! body :create-fixture)
         (modify-filter))
    (doto body
      (body-position! (- x half-width) (- y half-height) 0)
      (body! :set-linear-velocity 0 bullet-speed))
    body))

(defn create-bullet!
  [screen x y]
  (let [bullet (shape :filled :set-color (color :white) :rect 0 0 bullet-width bullet-height)]
    (sound "shot3.ogg" :play)
    (assoc bullet
      :id :bullet
      :bullet? true :render-layer 50
      :x (- x half-width)
      :y (- y half-height)
      :body (create-bullet-body! screen x y)
      :width bullet-width :height bullet-height)))

