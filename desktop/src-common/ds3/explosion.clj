(ns ds3.explosion
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [ds3.common :as c]
            [clojure.pprint :refer [pprint]]))

(def ^:const x-offset (c/screen-to-world -3))

(def ^:const y-offset (c/screen-to-world 2))

(defn create-circle-texture [c]
  (let [pix-map (pixmap* 16 16 (pixmap-format :r-g-b-a8888))]
    (doto pix-map
      (pixmap! :set-color c)
      (pixmap! :fill-circle 8 8 7.5))
    (texture pix-map)))

(defn create-circle-entity [x y width radius c]
  (let [circle-entity (create-circle-texture c)]
    (assoc circle-entity
      :width (c/screen-to-world (* 2 width)) :height (c/screen-to-world (* 2 width))
      :x (+ x x-offset (c/screen-to-world  (- 8 width))) :y (+ y y-offset (c/screen-to-world (- 8 width)))
      :ttl 12 :frame-ticks 1
      )))

(defn create-explosion [x y]
  (let [circle-texture (create-circle-entity x y 8 7.5 (color :black))
        circle-texture2 (create-circle-entity x y 7 6.5 (color :orange))
        circle-texture3 (create-circle-entity x y 6 5.5 (color :yellow))
        circle-texture4 (create-circle-entity x y 1 1.5 (color :white))
        explosion (bundle circle-texture circle-texture2 circle-texture3 circle-texture4)]
    (assoc explosion :explosion? true)))

(defn handle-explosion [{:keys [entities] :as explosion}]
  (let [[part1 part2 part3 {:keys [x y width height ttl frame-ticks] :as part4}] entities]
    ;(clojure.pprint/pprint part4)
    ;(prn :x x :y y :width width :height height :ttl ttl :frame-ticks frame-ticks)
    (cond (and (> ttl 0) (= 0 frame-ticks))
          (assoc explosion :entities [part1 part2 part3 (assoc part4
                                                          :x (- x (c/screen-to-world 0.5)) :y (- y (c/screen-to-world 0.5))
                                                          :width (+ width (c/screen-to-world 1)) :height (+ height (c/screen-to-world 1))
                                                          :ttl (- ttl 1) :frame-ticks 1)])
          (and (> ttl 0) (> frame-ticks 0))
          (assoc explosion :entities [part1 part2 part3 (assoc part4 :frame-ticks (- frame-ticks 1))])

    )))
