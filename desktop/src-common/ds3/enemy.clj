(ns ds3.enemy
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [ds3.common :as c]
            [ds3.explosion :as exp]
            [ds3.ship :as ship]
            [ds3.splines :as splines]
            [play-clj.core :refer [bundle shape color key-pressed? pixmap! pixmap* update! x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer [ b-spline b-spline! vector-2 vector-2!]])
  (:import [com.badlogic.gdx.graphics Pixmap Texture TextureData Pixmap$Format]))

(declare create-enemy-body! update-drift update-from-spline update-home update-returning)

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

(def state-machine {:drifting :attacking :attacking :returning :returning :drifting})

(def starting-state :drifting)

(def speed (c/screen-to-world 6))
(def returning-speed (c/screen-to-world 0.6))
(def d-time (/ 1.0 60))
returning-speed

(defn create-enemy-entity! [screen ship-texture col]
  (let [pixel-ship (texture ship-texture)]
    (doto (assoc pixel-ship
            :body (create-enemy-body! screen)
            :width (c/screen-to-world 16) :height (c/screen-to-world 16)
            :id :enemy-ship :enemy? true :render-layer 70 :score 100
            :translate-x (- (c/screen-to-world c/ship-mp-xoffset)) :translate-y (- (c/screen-to-world c/ship-mp-yoffset))
            :drift-x-delta (* (c/distance-from-center col) c/drift-x-delta)
            :drift-y-delta (/ (* (* (c/distance-from-center col) (c/distance-from-center col)) c/drift-x-delta) 20.0)
            :movement-state starting-state)
        (body! :set-linear-velocity 0 0))))

(defn create-enemy-body!
  [screen]
  (let [body (add-body! screen (body-def :static))]
    (->> (polygon-shape :set-as-box (c/screen-to-world 3) (c/screen-to-world 3) (vector-2 0 0) 0)
         (fixture-def :density 1 :friction 0 :restitution 1 :is-sensor true :shape)
         (body! body :create-fixture))
    body))


(defn create-enemies [screen]
  (let [ship-textures (into [] (take c/enemy-height (repeatedly #(ship/create-pixel-ship-texture (rand-int Integer/MAX_VALUE)))))]
         (for [row (range c/enemy-rows)
               col (range c/enemy-columns)
               :let [x (+ c/enemy-start-x (* col c/enemy-width-start))
                     y (+ c/enemy-start-y (* row c/enemy-height))]]
           (doto (assoc (create-enemy-entity! screen (nth ship-textures row) col) :row row :col col :home-x (c/screen-to-world x) :home-y (c/screen-to-world y))
             (body-position! (c/screen-to-world x) (c/screen-to-world y) 0)
             ))))

(defn move [entity screen]
  (let [ms (:movement-state entity)
        entity (update-home entity screen)]
    (cond (= :drifting ms)
          (update-drift entity screen)
          (= :attacking ms)
          (update-from-spline entity)
          (= :returning ms)
          (update-returning entity screen))
    ))

(defn handle-collision [enemy other-entity screen entities]
  (cond (:bullet? other-entity)
        (cond (= :drifting (:movement-state enemy))
              (do
                (let [entities (->> entities
                     (map (fn [entity]
                            (cond (= enemy entity)
                                  (assoc entity :movement-state (state-machine (:movement-state entity)) :current-time 0
                                    :spline (splines/calibrate-spline (:x entity) (:y entity)))
                                  :else entity)))
                     )]
                  (remove #(= other-entity %) entities)))
              (= :attacking (:movement-state enemy))
              (do
                (update! screen :level-score (+ (:level-score screen) (:score enemy)))
                ;(screen! hud/hud-screen :on-update-score :score (+ (:level-score screen) (:score enemy)))
                (remove #(or (= enemy %)
                             (= other-entity %))
                        (conj entities (exp/create-explosion (:x enemy) (:y enemy)))))
              :else nil
              )))

(defn update-home [entity screen]
  (let [on-left (< (:home-x entity) c/half-game-width-world)
        outward  (:formation-expand screen)
        b (cond on-left outward
                :else (not outward))
        delta-x-fn (cond b -
                         :else +)
        delta-y-fn (cond outward -
                         :else +)]
    (assoc entity :home-x (delta-x-fn (:home-x entity) (:drift-x-delta entity)) :home-y (delta-y-fn (:home-y entity) (:drift-y-delta entity)))))

(defn update-drift [entity screen]
  (body-position! entity (:home-x entity) (:home-y entity) (:angle entity))
  entity);)

(defn update-from-spline [entity]
  (let [current-time (if (> (:current-time entity) 1)
                         (- (:current-time entity) 1)
                         (:current-time entity))
        spline (:spline entity)
        v (b-spline! spline :value-at (vector-2 0 0) current-time)
        dv (b-spline! spline :derivative-at (vector-2 0 0) current-time)
        l (vector-2! dv :len)
        a (- (vector-2! dv :angle) 90)
        new-delta (/ (* d-time speed) l)
        x (x v)
        y (y v)]
    (cond (> (:current-time entity) 1)
          (do
            (body-position! entity (:home-x entity) (c/screen-to-world c/game-height) 0)
            (assoc entity :movement-state (state-machine (:movement-state entity))))
          :else
          (do
            (body-position! entity x y a)
            (assoc entity :current-time (+ current-time new-delta)))
          )
    ))

(defn update-returning [{:keys [:home-x :home-y] :as entity} screen]
  (let [cur-pos (vector-2 (:x entity) (:y entity))
        target-pos (vector-2 home-x home-y)
        dir-vec (vector-2! target-pos :sub cur-pos)
        l (vector-2! dir-vec :len)
        ;delta-vec (vector-2! dir-vec :set-length returning-speed)
        ]
    (vector-2! dir-vec :set-length returning-speed)
    (cond (< l returning-speed)
          (do
            (body-position! entity home-x home-y 0)
            (assoc entity :movement-state (state-machine (:movement-state entity))))
          :else
          (do
            (body-position! entity (+ (:x entity) (x dir-vec)) (+ (:y entity) (y dir-vec)) 0)
            entity))
    ))
