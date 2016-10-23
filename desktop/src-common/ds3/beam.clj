(ns ds3.beam
  (:require [play-clj.core :refer [bundle shape sound]]
            [play-clj.g2d :refer [animation animation->texture texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! chain-shape circle-shape fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [ds3.common :as c]))

(declare create-beam-body! create-beam-entity!)

(def beam-width (c/screen-to-world 20))
(def beam-height (c/screen-to-world 10))
(def beam-y (c/screen-to-world (/ c/game-height 15)))

(defn create-beam [screen x y]
  (let [tractor-beam (doto (create-beam-entity! screen beam-width beam-height)
                       (body-position! (- x (/ beam-width 2)) (- beam-y (/ beam-height 2)) 0))]
    tractor-beam))

(defn- create-beam-entity!
  [screen width height]
  (let [rect (bundle nil)]
    (assoc rect
      :body (create-beam-body! screen width height)
      :width width :height height
      :beaming-ticks c/beaming-ticks
      :beam? true)))

(defn- create-beam-body! [screen width height]
  (let [body (add-body! screen (body-def :static))
        half-width (/ width 2)
        half-height (/ height 2)]
    (->> (polygon-shape :set-as-box half-width half-height(vector-2 half-width half-height) 0)
         (fixture-def :density 1 :friction 0 :restitution 1 :is-sensor true :shape)
         (body! body :create-fixture ))
    body))

(defn handle-beam [{:keys [:beaming-ticks] :as entity}]
  (let [new-ticks (- beaming-ticks 1)]
    (if (= 0 beaming-ticks)
      nil
      (assoc entity :beaming-ticks new-ticks))))
