(ns ds3.space
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format sound]]
            [play-clj.g2d :refer [texture]]
            [ds3.common :as c]))

(def star-count 100)
(def star-speed-slow (c/screen-to-world -0.1))
(def star-speed-med (c/screen-to-world -0.2))
(def star-speed-fast (c/screen-to-world -0.4))
(def star-speeds [star-speed-slow star-speed-med star-speed-fast])
(def star-alpha 0.75)
(def star-color-degree 0.75)
(def star-colors [(color 1 1 1 star-alpha) (color 1 1 star-color-degree star-alpha) (color 1 star-color-degree 1 star-alpha)
                  (color star-color-degree 1 1 star-alpha) (color star-color-degree star-color-degree  1 star-alpha)
                  (color star-color-degree 1 star-color-degree star-alpha) (color 1 star-color-degree star-color-degree star-alpha)])

(defn create-star-texture [c]
  (let [pix-map (pixmap* 1 1 (pixmap-format :r-g-b-a8888))]
    (doto pix-map
      (pixmap! :set-color c)
      (pixmap! :fill-rectangle 0 0 1 1))
    (texture pix-map)))

(defn create-star [x y]
  (assoc (create-star-texture (first (shuffle star-colors)))
    :width (c/screen-to-world 1) :height (c/screen-to-world 1)
    :x x :y y
    :star? true :id :star
    :render-layer 1
    :speed (first (shuffle star-speeds))))

(defn create-space []
  (for [count (range star-count)]
    (create-star (c/screen-to-world (rand-int c/game-width)) (c/screen-to-world (rand-int c/game-height)))
    ))

(defn move-star [screen {:keys [:y :speed] :as entity}]
  (let [new-y (if (< (+ y speed) 0)
                    (+ (c/screen-to-world 5) (c/screen-to-world c/game-height))
                    (+ y speed))]
  (assoc entity :y new-y)))


