(ns ds3.beam
  (:require [play-clj.core :refer [bundle color shape sound]]
            [play-clj.g2d :refer [animation animation->texture texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! chain-shape circle-shape fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [ds3.common :as c]))

(declare create-beam-body! create-beam-entity!)

(def beam-width (c/screen-to-world 20))
(def beam-height (c/screen-to-world 10))
(def beam-y (c/screen-to-world (/ c/game-height 15)))
(def beam-color (color 1.0 1.0 1.0 0.1))

(defn create-beam [screen x y]
  (let [tractor-beam (doto (create-beam-entity! screen x y)
                       (body-position! (- x (/ beam-width 2)) (- beam-y (/ beam-height 2)) 0))]
    tractor-beam))

(defn- create-beam-entity!
  [screen x y]
  (let [rect2 (bundle nil)
        rect (shape :filled :set-color beam-color
                    :rect 0 0 beam-width (- y (c/screen-to-world c/enemy-height)))]
    (assoc rect
      :body (create-beam-body! screen beam-width beam-height)
      :x (- x (/ beam-width 2)) :y (- beam-y (/ beam-height 2))
      :width beam-width :height beam-height
      :render-layer 60
      :beaming-ticks c/beaming-ticks
      :beam? true :id :beam)))

(defn- create-beam-body! [screen width height]
  (let [body (add-body! screen (body-def :dynamic))
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
      (do
        (body! entity :apply-force-to-center (vector-2 0 0) true)
        (assoc entity :beaming-ticks new-ticks)))))

(defn handle-collision [beam other-entity screen entities]
  (do
    (cond (:ship? other-entity)
        (do
          (prn :captured!)
          entities)
        :else entities)))
