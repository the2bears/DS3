(ns ds3.ship
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [ds3.common :as c]
            [ds3.explosion :as exp]
            [ds3.spark :as spark]
            [play-clj.core :refer [add-timer! bundle shape color key-pressed? pixmap! pixmap* pixmap-format screen! update! x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer [vector-2 vector-2!]]))

(declare create-ghost-body! create-pixel-ship-texture create-ship-body! draw-rect-pixelmap hsv-to-rgb  move play-clj-color update-captured)

(def speed (c/screen-to-world 1.5))
(def tractor-beam-speed (c/screen-to-world 0.75))
(def drop-speed (c/screen-to-world 1.1))
(def default-r2 (c/screen-to-world 1.0))

(defn create-ship-entity! [screen]
  (let [pixel-ship (create-pixel-ship-texture Integer/MAX_VALUE)];c/ghost-color)]
    (update! screen :ship-x (c/screen-to-world (/ c/game-width 2)))
    (doto (assoc pixel-ship
            :body (create-ship-body! screen)
            :width (c/screen-to-world 16) :height (c/screen-to-world 16)
            :id :pixel-ship :ship? true :render-layer 90
            :captured? false
            :translate-x (- (c/screen-to-world c/ship-mp-xoffset)) :translate-y (- (c/screen-to-world c/ship-mp-yoffset)))
      (body-position! (c/screen-to-world (/ c/game-width 2)) c/ship-y-default 0)
      (body! :set-linear-velocity 0 0))))

(defn- create-ship-body!
  [screen]
  (let [body (add-body! screen (body-def :dynamic))
        ship-shape (polygon-shape :set-as-box (c/screen-to-world 2) (c/screen-to-world 2) (vector-2 0 0) 0)]
    (->> ship-shape
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    (.dispose ship-shape)
    body))

(defn create-ghost-entity! [screen x y]
  (let [ghost-ship (create-pixel-ship-texture Integer/MAX_VALUE c/ghost-color)]
    (doto (assoc ghost-ship
            :body (create-ghost-body! screen)
            :width (c/screen-to-world 16) :height (c/screen-to-world 16)
            :id :ghost-ship :ghost? true :render-layer 90
            :below? true :shift-position? false :capture-height (c/screen-to-world c/capture-height)
            :translate-x (- (c/screen-to-world c/ship-mp-xoffset)) :translate-y (- (c/screen-to-world c/ship-mp-yoffset)))
      (body-position! x y 0)
      (body! :set-linear-velocity 0 0))))

(defn- create-ghost-body!
  [screen]
  (let [body (add-body! screen (body-def :dynamic))
        ship-shape (polygon-shape :set-as-box (c/screen-to-world 2) (c/screen-to-world 2) (vector-2 0 0) 0)]
    (->> ship-shape
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    (.dispose ship-shape)
    body))

(defn- create-pixel-map-list
  ([seed c-model]
   (let [ship-map (psc/color-pixel-ship (psc/create-pixel-ship (assoc bollinger/model :seed seed)) c-model)
         tags (keys (:pixels ship-map))
         pixels (:pixels ship-map)
         shape-builder (fn[s] (reduce (fn[acc n] (conj acc n)) [] s))]
     (reduce (fn[acc tag](concat (shape-builder (tag pixels)) acc)) [] tags))))

(defn create-pixel-ship-texture
  ([]
   (create-pixel-ship-texture (rand-int Integer/MAX_VALUE)))
  ([seed]
   (create-pixel-ship-texture seed bollinger/color-scheme))
  ([seed c-scheme]
   (let [pixel-map-list (create-pixel-map-list seed c-scheme)
         pix-map (pixmap* 16 16 (pixmap-format :r-g-b-a8888))]
     (doseq [pixel pixel-map-list] (draw-rect-pixelmap pix-map pixel))
     (assoc (texture pix-map) :seed seed))))

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

(defn move-player-tick [screen {:keys [:x :y :angle] :as entity}]
  (if (:ship? entity)
    (let [y-diff (- y c/ship-y-default)]
      (cond (:captured? entity)
            (update-captured screen entity)
            :else
            (cond
              (key-pressed? :dpad-right)
              (move screen entity :right)
              (key-pressed? :dpad-left)
              (move screen entity :left)
              (> y-diff 0)
              (move screen entity :straight)
              :else entity)))
    entity))

(defn- move [screen {:keys [:x :y :angle] :as entity} direction]
  (let [mv-fn (case direction
                :right +
                :left -
                :straight (fn[x _] x))
        x (mv-fn x speed)
        x-anchored (cond (> x c/game-width-adj) c/game-width-adj
                         (< x 0) 0
                         :else x)
        y-diff (- y c/ship-y-default)
        y-anchored (cond (> y-diff drop-speed) (- y drop-speed)
                         :else c/ship-y-default)]
    (body-position! entity x-anchored y-anchored angle)
    (update! screen :ship-x x-anchored))
  entity)

(defn- update-captured [screen {:keys [:captured-x :captured-y] :as entity}]
  (let [cur-pos (vector-2 (:x entity) (:y entity))
        target-pos (vector-2 captured-x captured-y)
        dir-vec (vector-2! target-pos :sub cur-pos)
        l (vector-2! dir-vec :len)]
    (vector-2! dir-vec :set-length tractor-beam-speed)
    (cond (< l tractor-beam-speed)
          (do
            (update! screen :can-attack? false)
            (add-timer! screen :start-towing 0)
            (body-position! entity captured-x captured-y 0)
            (create-ghost-entity! screen captured-x captured-y))
          :else
          (do
            (body-position! entity (+ (:x entity) (x dir-vec)) (+ (:y entity) (y dir-vec)) 0)
            entity))))

(defn handle-collision [ship other-entity screen entities]
  (cond (:bomb? other-entity)
        (let [lives (- (:p1-lives screen) 1)]
          (update! screen :can-attack? false :p1-rank c/starting-rank :p1-bonus 1)
          (remove #(or (= other-entity %) (= ship %)) (conj entities (exp/create-ship-explosion (:x ship) (:y ship)))))
        (:mini? other-entity)
        (let [lives (- (:p1-lives screen) 1)]
          (update! screen :can-attack? false :p1-rank c/starting-rank :p1-bonus 1)
          (remove #(or (= other-entity %) (= ship %)) (conj entities
                                                            (exp/create-ship-explosion (:x ship) (:y ship))
                                                            (exp/create-explosion (:x other-entity) (:y other-entity)))))
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
      (cond collided? (let [enemy (first dead-enemies)]
                        (if (some? (:spark-emitter enemy))
                          (spark/remove-spark-emitter (:spark-emitter enemy)))
                        (update! screen :can-attack? false :p1-rank c/starting-rank :p1-bonus 1)
                        (remove #(or (= enemy %) (= ship %)) (conj entities
                                                                   (exp/create-ship-explosion (:x ship) (:y ship))
                                                                   (exp/create-explosion (:x enemy) (:y enemy)))))
            :else entities))
    entities
    ))

(defn handle-ghost [screen entities ghost]
  (let [master (first (filter #(:master? %) entities))]
    (if master
      (let [captured-x (:x master)
            captured-y (:y master)
            capture-height (:capture-height ghost)
            shifting (and (:shift-position? ghost) (> capture-height (c/screen-to-world c/capture-height-shifted)))
            start-shifting (and (:below? ghost) (not shifting) (= :drifting (:movement-state master)))
            stop-shifting (and shifting (< capture-height (c/screen-to-world c/capture-height-shifted)))
            new-ghost (cond start-shifting (assoc ghost :shift-position? true :render-layer 60 :below? false)
                            shifting (assoc ghost :capture-height (- capture-height tractor-beam-speed))
                            stop-shifting (assoc ghost :shift-position? false)
                            :else ghost)]
        (body-position! new-ghost captured-x (- captured-y (:capture-height new-ghost)) 0)
        new-ghost)
      (do
        ;(prn :handle-ghost :no-master)
        ghost))))
