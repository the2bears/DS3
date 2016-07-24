(ns ds3.enemy
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [ds3.common :as c]
            [ds3.ship :as ship]
            [play-clj.core :refer [bundle shape color key-pressed? pixmap! pixmap*]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer [vector-2]])
  (:import [com.badlogic.gdx.graphics Pixmap Texture TextureData Pixmap$Format]))

(declare create-enemy-body!)

(def boss
  {:name :ds3-boss
   :seed Integer/MAX_VALUE
   :ship-size 12
   :model {:solid [{:x 5, :y 3} {:x 5, :y 4} {:x 5, :y 5} {:x 5, :y 9}],
           :cockpit [{:x 4, :y 6} {:x 5, :y 6} {:x 4, :y 7} {:x 5, :y 7} {:x 4, :y 8} {:x 5, :y 8}],
           :hull [{:x 5, :y 2} {:x 1, :y 1} {:x 2, :y 1} {:x 1, :y 2} {:x 2, :y 2} {:x 1, :y 3} {:x 2, :y 3} {:x 1, :y 4} {:x 2, :y 4}
                  {:x 4, :y 2} {:x 3, :y 3} {:x 4, :y 3} {:x 3, :y 4} {:x 4, :y 4} {:x 2, :y 5} {:x 3, :y 5}
                  {:x 4, :y 5} {:x 1, :y 6} {:x 2, :y 6} {:x 3, :y 6} {:x 1, :y 7} {:x 2, :y 7} {:x 3, :y 7} {:x 1, :y 8} {:x 2, :y 8}
                  {:x 3, :y 8} {:x 1, :y 9} {:x 2, :y 9} {:x 3, :y 9} {:x 4, :y 9} {:x 3, :y 10} {:x 4, :y 10} {:x 5, :y 10}]}})


(defn create-enemy-entity! [screen seed]
  (let [pixel-ship (ship/create-pixel-ship-texture seed)]
    (doto (assoc pixel-ship
            :body (create-enemy-body! screen)
            :width (c/screen-to-world 16) :height (c/screen-to-world 16)
            :id :enemy-ship :enemy? true)
        (body! :set-linear-velocity 0 0))))

(defn create-enemy-body!
  [screen]
  (let [body (add-body! screen (body-def :static))]
    (->> (polygon-shape :set-as-box (c/screen-to-world 2) (c/screen-to-world 2) (vector-2 (c/screen-to-world c/ship-mp-xoffset) (c/screen-to-world c/ship-mp-yoffset)) 0)
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    body))

