(ns ds3.bomb
  (:require [play-clj.core :refer [color shape sound]]
            [play-clj.g2d :refer [animation animation->texture texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! circle-shape fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [ds3.common :as c])
  (:import [com.badlogic.gdx.physics.box2d Filter]))


(def bomb-width (c/screen-to-world 3.0))
(def bomb-height (c/screen-to-world 4.0))
(def bomb-speed (c/screen-to-world -28.0))
(def half-width (/ bomb-width 2))
(def half-height (/ bomb-height 2))


(defn animate-bomb [screen {:keys [bomb-animation total-time] :as entity}]
  (let [delta (:total-time screen)
        time-keeper (if (:bomb? entity) {:total-time (+ total-time delta)}
                                        {:total-time delta})]
    (merge entity (animation->texture time-keeper bomb-animation))))

(defn create-bomb-body! [screen x y]
  (let [body (add-body! screen (body-def :dynamic))]
    (->> (polygon-shape :set-as-box half-width half-height (vector-2 half-width (+ bomb-height half-height)) 0)
         (fixture-def :density 1 :friction 0 :restitution 1 :is-sensor true :shape)
         (body! body :create-fixture ))
    (doto body
      (body-position! x y 0)
      (body! :set-linear-velocity 0 bomb-speed))
    body))

(defn create-bomb [screen x y]
  (let [bomb (texture "bomb.png")
        bomb1 (texture bomb :set-region 0 0 3 4)
        bomb2 (texture bomb :set-region 3 0 3 4)
        bomb-animation (animation 0.2 [bomb1 bomb2])
        animation-delta (/ (rand-int 1000) 1000.0)]
    (sound "bomb2.ogg" :play)
    (assoc bomb1 :bomb-animation (animation 0.1 [bomb1 bomb2]) :total-time animation-delta
      :bomb? true :width bomb-width :height bomb-height :id :bomb :x x :y y
      :render-layer 75 :translate-x 0 :translate-y bomb-height
      :body (create-bomb-body! screen x y))))
