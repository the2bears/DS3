(ns ds3.splines
  (:require [ds3.common :as c]
            [play-clj.core :refer [color defscreen game orthographic render! shape stage update! x y]]
            [play-clj.g2d-physics :refer [body-position!]]
            [play-clj.math :refer [bezier bezier! b-spline b-spline! catmull-rom-spline catmull-rom-spline! vector-2 vector-2!]]
            [play-clj.ui :refer [label label!]]))

(declare calibrate-spline points-to-vector-2 update-spline)

(def speed (c/screen-to-world 6.0))

(def d-time (/ 1.0 60))

(def ^:const hook-near-x (c/screen-to-world (* c/game-width (/ 50.0 800.0))));button hook distance, and distance on close side
(def ^:const hook-far-x (c/screen-to-world (* c/game-width (/ 100.0 800.0))));x padding on far side
(def ^:const hook-width-near-x (c/screen-to-world (- c/game-width (* c/game-width (/ 50.0 800.0)))));button hook distance, and distance on close side
(def ^:const hook-width-far-x (c/screen-to-world (- c/game-width (* c/game-width (/ 100.0 800.0)))));x padding on far side

(defn points-to-vector-2 [p]
  (into [] (map (fn[[x y]]
       (vector-2 x y)) p)))

(defn calibrate-spline [x y]
  (let [w (c/screen-to-world c/game-width)
        h (c/screen-to-world c/game-height)
        hw (/ w 2.0)
        x-left (< x hw)
        x-fn (if x-left + -)
        x1 (if x-left hook-near-x hook-width-near-x)
        x2 (if x-left hook-width-far-x hook-far-x)
        x3 (if x-left 0 (c/screen-to-world c/game-width))
        ty (- y hook-near-x)
        dy (/ (- ty hook-near-x) 4.0)
        points [[x y][x y][x (+ y hook-near-x)][(x-fn x hook-near-x) (+ y hook-near-x)][(x-fn x hook-near-x) ty]
                [x1 (- ty dy)][x2 (- ty (* 2 dy))][x2 (- ty (* 3 dy))][x1 hook-near-x][x3 (- hook-far-x)][x3 (- hook-far-x)]]
        points-vec (points-to-vector-2 points)
        spline (catmull-rom-spline points-vec false)]
    spline))



(defn update-from-spline [entity]
  (let [delta-time d-time]
    (let [current-time (if (> (:current-time entity) 1)
                         (- (:current-time entity) 1)
                         (:current-time entity))
          spline (:spline entity)
          v (catmull-rom-spline! spline :value-at (vector-2 0 0) current-time)
          dv (catmull-rom-spline! spline :derivative-at (vector-2 0 0) current-time)
          l (vector-2! dv :len)
          a (- (vector-2! dv :angle) 90)
          new-delta (/ (* delta-time speed) l)
          x (x v)
          y (y v)]
      (body-position! entity x y a)
      (assoc entity
        ;:x x :y y :angle a
        :current-time (+ current-time new-delta)))
    ))

