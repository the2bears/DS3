(ns ds3.ship
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [ds3.common :as c]
            [ds3.explosion :as exp]
            [play-clj.core :refer [bundle shape color key-pressed? pixmap! pixmap* pixmap-format screen! update!]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer [vector-2 vector-2!]]))

(declare play-clj-color hsv-to-rgb create-ship-body! move draw-rect-pixelmap create-pixel-ship-texture)

(def speed (c/screen-to-world 1.5))
(def default-r2 (c/screen-to-world 1.0))

(defn create-ship-entity! [screen]
  (let [pixel-ship (create-pixel-ship-texture Integer/MAX_VALUE)]
    (doto (assoc pixel-ship
            :body (create-ship-body! screen)
            :width (c/screen-to-world 16) :height (c/screen-to-world 16)
            :id :pixel-ship :ship? true :render-layer 90
            :translate-x (- (c/screen-to-world c/ship-mp-xoffset)) :translate-y (- (c/screen-to-world c/ship-mp-yoffset)))
      (body-position! (c/screen-to-world (/ c/game-width 2)) (c/screen-to-world (/ c/game-height 15)) 0)
      (body! :set-linear-velocity 0 0))))

(defn create-ship-body!
  [screen]
  (let [body (add-body! screen (body-def :dynamic))
        ship-shape (polygon-shape :set-as-box (c/screen-to-world 2) (c/screen-to-world 2) (vector-2 0 0) 0)]
    (->> ship-shape
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    (.dispose ship-shape)
    body))

(defn create-pixel-map-list
  ([]
   (create-pixel-map-list (rand-int Integer/MAX_VALUE)))
  ([seed]
   (let [ship-map (psc/color-pixel-ship (psc/create-pixel-ship (assoc bollinger/model :seed seed)))
         tags (keys (:pixels ship-map))
         pixels (:pixels ship-map)
         shape-builder (fn[s] (reduce (fn[acc n] (conj acc n)) [] s))]
     (reduce (fn[acc tag](concat (shape-builder (tag pixels)) acc)) [] tags))))

(defn create-pixel-ship-texture
  ([]
   (create-pixel-ship-texture (rand-int Integer/MAX_VALUE)))
  ([seed]
   (let [pixel-map-list (create-pixel-map-list seed)
         pix-map (pixmap* 16 16 (pixmap-format :r-g-b-a8888))]
     (doseq [pixel pixel-map-list] (draw-rect-pixelmap pix-map pixel))
     (texture pix-map))))

(def p-per-r 1)

(defn draw-rect-pixelmap [pix-map {:keys [x y color]}]
  (let [c (play-clj-color color)]
    (doto pix-map
      (pixmap! :set-color c)
      (pixmap! :fill-rectangle x y p-per-r p-per-r))))

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

(defn move-player-tick [screen entity]
  (if (:ship? entity)
    (cond
      (key-pressed? :dpad-right)
        (move screen entity :right)
      (key-pressed? :dpad-left)
        (move screen entity :left)
     :else entity)
    entity)
  )

(defn move [screen {:keys [:x :y :angle] :as entity} direction]
  (let [mv-fn (case direction
                :right +
                :left -)
        x (mv-fn x speed)
        x-anchored (cond (> x c/game-width-adj) c/game-width-adj
                         (< x 0) 0
                         :else x)]
    (body-position! entity x-anchored y angle)
    (update! screen :ship-x x-anchored))
  entity)

(defn handle-collision [ship other-entity screen entities]
  (cond (:bomb? other-entity)
        (let [lives (- (:p1-lives screen) 1)]
          (update! screen :can-attack? false)
          (remove #(or (= other-entity %) (= ship %)) (conj entities (exp/create-ship-explosion (:x ship) (:y ship)))))
        :else entities))

(defn collide-with-enemy? [screen entities]
  (if-let [ship (first (filter #(:ship? %) entities))]
    (let [enemies (filter #(:enemy? %) entities)
          ship-pos (body! ship :get-position)
          collide? (fn [ship enemy]
                     (let [enemy-pos (body! enemy :get-position)
                           distance (vector-2! ship-pos :dst2 enemy-pos)]
                       (< distance default-r2)))
          dead-enemies (filter #(collide? ship %) enemies)
          collided? (> (count dead-enemies) 0)]
      (cond collided? (do
                        (update! screen :can-attack? false)
                        (remove #(or (= (first dead-enemies) %) (= ship %)) (conj entities
                                                                                  (exp/create-ship-explosion (:x ship) (:y ship))
                                                                                  (exp/create-explosion (:x (first dead-enemies)) (:y (first dead-enemies))))))
            :else entities))
    entities
    ))
