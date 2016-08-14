(ns ds3.bomb
  (:require [play-clj.core :refer [color shape sound]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [ds3.common :as c]
            )
  (:import [com.badlogic.gdx.physics.box2d Filter]))

(defn modify-filter [fixture]
  (let [bullet-filter (fixture! fixture :get-filter-data)]
    (set! (.groupIndex bullet-filter) bullet-filter-group)
    (fixture! fixture :set-filter-data bullet-filter)))

(defn animate-bomb [screen {:keys [bomb-animation total-time] :as entity}]
  (let [delta (:total-time screen)
        time-keeper (if (:bomb? entity) {:total-time (+ total-time delta)}
                                        {:total-time delta})]
    (if (:bomb? entity)
      (merge entity (animation->texture time-keeper bomb-animation))
    entity)))

(defn create-bomb-body! [screen]
  (let [body (add-body! screen (body-def :dynamic :bullet false))]
    (->> (circle-shape :set-radius 5)
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture ))

    (doto body
      (body-position! (rand-int 400) (rand-int 400) (cond (= 0 (rand-int 2)) 180
                                                          :else 0))
      (body! :set-linear-velocity 0 0))
    body))

(defn create-bomb [screen]
  (let [bomb (texture "bomb.png")
        bomb1 (texture bomb :set-region 0 0 3 4)
        bomb2 (texture bomb :set-region 3 0 3 4)
        bomb-animation (animation 0.2 [bomb1 bomb2])
        animation-delta (/ (rand-int 1000) 1000.0)]
    (assoc bomb1 :bomb-animation (animation 0.1 [bomb1 bomb2]) :total-time animation-delta
      :bomb? true :width 24 :height 32 ;:translate-x -10 :translate-y -5
      :body (create-bomb-body! screen))))
