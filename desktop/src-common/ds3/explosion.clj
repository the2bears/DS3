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
(def yellows [(color :black)(color :orange)(color :yellow)(color :white)])
(def blues [(color :black)(color :blue)(color :cyan)(color :white)])
(def enemy-explosion-textures (atom []))
(def ship-explosion-textures (atom []))

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
      )))

(defn- internal-explosion [x y textures-atom colors]
  (let [textures (cond (empty? @textures-atom)
                       (reset! textures-atom
                               [(create-circle-entity 8 7.5 (colors 0))
                                (create-circle-entity 7 6.5 (colors 1))
                                (create-circle-entity 6 5.5 (colors 2))
                                (create-circle-entity 1 1.5 (colors 3))])
                       :else @textures-atom)
        explosion (bundle
                    (assoc (textures 0) :x (- x layer1-width) :y (- y layer1-width) :ttl 12 :frame-ticks default-frame-ticks)
                    (assoc (textures 1) :x (- x layer2-width) :y (- y layer2-width) :ttl 12 :frame-ticks default-frame-ticks)
                    (assoc (textures 2) :x (- x layer3-width) :y (- y layer3-width) :ttl 12 :frame-ticks default-frame-ticks)
                    (assoc (textures 3) :x (- x layer4-width) :y (- y layer4-width) :ttl 12 :frame-ticks default-frame-ticks))]
    (sound "explosion.ogg" :play)
    (assoc explosion :explosion? true :render-layer 100)))

(defn create-explosion [x y]
  (internal-explosion x y enemy-explosion-textures blues))

(defn create-ship-explosion [x y]
  (internal-explosion x y ship-explosion-textures yellows))

(defn handle-explosion [{:keys [entities] :as explosion}]
  (let [[layer1 layer2 layer3 {:keys [x y width height ttl frame-ticks] :as layer4}] entities]
    (cond (and (> ttl 0) (= 0 frame-ticks))
          (assoc explosion :entities [layer1 layer2 layer3 (assoc layer4
                                                             :x (- x (c/screen-to-world 0.5)) :y (- y (c/screen-to-world 0.5))
                                                             :width (+ width (c/screen-to-world 1)) :height (+ height (c/screen-to-world 1))
                                                             :ttl (- ttl 1) :frame-ticks default-frame-ticks)])
          (and (> ttl 0) (> frame-ticks 0))
          (assoc explosion :entities [layer1 layer2 layer3 (assoc layer4 :frame-ticks (- frame-ticks 1))])

    )))



