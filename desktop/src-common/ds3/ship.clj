(ns ds3.ship
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [ds3.common :as c]
            [play-clj.core :refer [bundle shape color key-pressed?]]
            [play-clj.g2d-physics :refer :all]))

(declare custom-shape play-clj-color hsv-to-rgb create-ship-body! create-pixel-ship-play-clj move)

(def speed (c/screen-to-world 1.5))

(defn create-ship-entity! [screen]
  (let [pixel-ship (bundle nil)]
    (doto (assoc pixel-ship
            :body (create-ship-body! screen (c/screen-to-world 3))
            :entities (create-pixel-ship-play-clj Integer/MAX_VALUE)
            :id :pixel-ship :ship? true)
        (body-position! (c/screen-to-world (/ c/game-width 2)) (c/screen-to-world (/ c/game-height 10)) 180)
        (body! :set-linear-velocity 0 0))))

(defn create-ship-body!
  [screen radius]
  (let [body (add-body! screen (body-def :dynamic))]
    (->> (circle-shape :set-radius radius)
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    body))

(defn create-pixel-ship-play-clj
  ([]
   (create-pixel-ship-play-clj (rand-int Integer/MAX_VALUE)))
  ([seed]
   (let [ship-map (psc/color-pixel-ship (psc/create-pixel-ship (assoc bollinger/model :seed seed)))
         tags (keys (:pixels ship-map))
         pixels (:pixels ship-map)
         shape-builder (fn[s] (reduce (fn[acc n] (conj acc (custom-shape n))) [] s))]
     (reduce (fn[acc tag](concat (shape-builder (tag pixels)) acc)) [] tags))))

(def p-per-c (c/screen-to-world 1))

(defn custom-shape [{:keys [x y color]}]
  (let [c (play-clj-color color)]
    (shape :filled :set-color c :rect (- (* x p-per-c) (* 6 p-per-c)) (- (* y p-per-c) (* 6 p-per-c)) p-per-c p-per-c)))

(defn play-clj-color
  ([{:keys [h s v]}]
   (let [[r g b a] (hsv-to-rgb [h s v 1])]
     (play-clj-color r g b a)))
  ([r g b a]
   (color r g b a)))

(defn hsv-to-rgb
  ;Convert hsv to rgb
  ;Inputs are floats 0<i<1
  ([[hue saturation value alpha]]
  (if (= saturation 0)
    [value value value alpha]
    (let [hue2 (cond (= 1.0 hue) 0.0
                     :else hue)
          h (int (* hue2 6.0))
          f (- (* hue2 6.0) h)
          p (* value (- 1 saturation))
          q (* value (- 1 (* f saturation)))
          t (* value (- 1 (* (- 1 f) saturation)))]
          (case h
            0 [value t p alpha]
            1 [q value p alpha]
            2 [p value t alpha]
            3 [p q value alpha]
            4 [t p value alpha]
            [value p q alpha])

    ))))

(defn move-player-tick [entity]
  (if (:ship? entity)
    (cond
      (key-pressed? :dpad-right)
        (move entity :right)
      (key-pressed? :dpad-left)
        (move entity :left)
     :else entity)
    entity)
  )

(defn move
  [entity direction]
  (case direction
    :right (body-position! entity (+ (:x entity) speed) (:y entity) (:angle entity))
    :left (body-position! entity (- (:x entity) speed) (:y entity) (:angle entity))
    )
  entity)
