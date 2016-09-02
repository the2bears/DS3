(ns ds3.explosion
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format sound]]
            [play-clj.g2d :refer [texture]]
            [ds3.common :as c]
            [clojure.pprint :refer [pprint]]))

(def ^:const x-offset (c/screen-to-world -3))
(def ^:const y-offset (c/screen-to-world 2))
(def ^:const default-frame-ticks 1)
(def ^:const layer1-width (c/screen-to-world 8))
(def ^:const layer2-width (c/screen-to-world 7))
(def ^:const layer3-width (c/screen-to-world 6))
(def ^:const layer4-width (c/screen-to-world 1))
(def explosion-textures (atom []))

(defn create-circle-texture [c]
  (let [pix-map (pixmap* 16 16 (pixmap-format :r-g-b-a8888))]
    (doto pix-map
      (pixmap! :set-color c)
      (pixmap! :fill-circle 8 8 7.5))
    (texture pix-map)))

(defn create-circle-entity [width radius c]
  (let [circle-entity (create-circle-texture c)]
    (assoc circle-entity
      :width (c/screen-to-world (* 2 width)) :height (c/screen-to-world (* 2 width))
      :ttl 12 :frame-ticks default-frame-ticks
      )))

(defn create-explosion [x y]
  (let [textures (cond (empty? @explosion-textures)
                       (reset! explosion-textures
                               [(create-circle-entity 8 7.5 (color :black))
                                (create-circle-entity 7 6.5 (color :orange))
                                (create-circle-entity 6 5.5 (color :yellow))
                                (create-circle-entity 1 1.5 (color :white))])
                       :else @explosion-textures)
        explosion (bundle
                    (assoc (textures 0) :x (- x layer1-width) :y (- y layer1-width))
                    (assoc (textures 1) :x (- x layer2-width) :y (- y layer2-width))
                    (assoc (textures 2) :x (- x layer3-width) :y (- y layer3-width))
                    (assoc (textures 3) :x (- x layer4-width) :y (- y layer4-width))
                    )]
    (sound "explosion.ogg" :play)
    (assoc explosion :explosion? true :render-layer 100)))

(defn handle-explosion [{:keys [entities] :as explosion}]
  (let [[part1 part2 part3 {:keys [x y width height ttl frame-ticks] :as part4}] entities]
    ;(clojure.pprint/pprint part4)
    ;(prn :x x :y y :width width :height height :ttl ttl :frame-ticks frame-ticks)
    (cond (and (> ttl 0) (= 0 frame-ticks))
          (assoc explosion :entities [part1 part2 part3 (assoc part4
                                                          :x (- x (c/screen-to-world 0.5)) :y (- y (c/screen-to-world 0.5))
                                                          :width (+ width (c/screen-to-world 1)) :height (+ height (c/screen-to-world 1))
                                                          :ttl (- ttl 1) :frame-ticks default-frame-ticks)])
          (and (> ttl 0) (> frame-ticks 0))
          (assoc explosion :entities [part1 part2 part3 (assoc part4 :frame-ticks (- frame-ticks 1))])

    )))



