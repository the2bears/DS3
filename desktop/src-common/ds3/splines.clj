(ns ds3.splines
  (:require [ds3.common :as c]
            [play-clj.core :refer [color defscreen game orthographic render! shape stage update! x y]]
            [play-clj.g2d-physics :refer [body-position!]]
            [play-clj.math :refer [bezier bezier! b-spline b-spline! catmull-rom-spline catmull-rom-spline! vector-2 vector-2!]]
            [play-clj.ui :refer [label label!]]))

(declare calibrate-spline points-to-vector-2 update-spline)

(def ^:const hook-near-x (c/screen-to-world (* c/game-width (/ 50.0 800.0))));button hook distance, and distance on close side
(def ^:const hook-far-x (c/screen-to-world (* c/game-width (/ 100.0 800.0))));x padding on far side
(def ^:const hook-width-near-x (c/screen-to-world (- c/game-width (* c/game-width (/ 50.0 800.0)))));button hook distance, and distance on close side
(def ^:const hook-width-far-x (c/screen-to-world (- c/game-width (* c/game-width (/ 100.0 800.0)))));x padding on far side
(def ^:const bottom-y (c/screen-to-world (* c/game-width (/ 100.0 800.0))))

(defn points-to-vector-2 [p]
  (into [] (map (fn[[x y]]
       (vector-2 x y)) p)))

(defn calibrate-spline [x y row]
  (let [w (c/screen-to-world c/game-width)
        h (c/screen-to-world c/game-height)
        hw (/ w 2.0)
        x-left (< x hw)
        x-fn (if x-left + -)
        alt-x-fn (if x-left - +)
        x1 (if x-left hook-near-x hook-width-near-x)
        x2 (if x-left hook-width-near-x hook-near-x)
        x3 (if x-left 0 (c/screen-to-world c/game-width))
        x4 (if x-left (c/screen-to-world c/game-width) 0)
        half-w (if x-left (/ w 2) (- (/ w 2)))
        ty (- y hook-near-x)
        dy (/ (- ty bottom-y) 5)
        which-spline (case row
                       ;loop
                       (0 1)[[x y][x y][x (+ y hook-near-x)][(alt-x-fn x hook-near-x) (+ y hook-near-x)][(alt-x-fn x hook-near-x) ty]
                             [(+ x half-w) (- ty (* 2 dy))][(+ x half-w) (- ty (* 6.2 dy))][x (- ty (* 6.2 dy))][x (- ty (* 2 dy))]
                             [(+ x half-w) (- ty (* 1 dy))][(+ x half-w) (- ty (* 7 dy))][(+ x half-w) (- ty (* 7 dy))]]
                       ;back-n-forth
                       (2 3) [[x y][x y][x (+ y hook-near-x)][(x-fn x hook-near-x) (+ y hook-near-x)][(x-fn x hook-near-x) ty]
                              [x1 (- ty dy)][x1 (- ty (* 3 dy))][x2 (- ty (* 4 dy))][x4 (- hook-far-x)][x4 (- hook-far-x)]]
                       ;back-n-forth-n-back
                       4 [[x y][x y][x (+ y hook-near-x)][(x-fn x hook-near-x) (+ y hook-near-x)][(x-fn x hook-near-x) ty]
                          [x1 (- ty dy)][x1 (- ty (* 2 dy))][x2 (- ty (* 3 dy))][x2 (- ty (* 4 dy))][x1 bottom-y][x3 (- hook-far-x)][x3 (- hook-far-x)]])
        points-vec (points-to-vector-2 which-spline)
        spline (b-spline points-vec 3 false)]
    spline))
