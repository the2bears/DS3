(ns ds3.enemy
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [ds3.bomb :as bomb]
            [ds3.common :as c]
            [ds3.explosion :as exp]
            [ds3.ship :as ship]
            [ds3.splines :as splines]
            [play-clj.core :refer [bundle shape color key-pressed? pixmap! pixmap* screen! update! x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer [ b-spline b-spline! vector-2 vector-2!]])
  (:import [com.badlogic.gdx.graphics Pixmap Texture TextureData Pixmap$Format]))

(declare create-enemy-body! create-mini-body! update-drift update-from-spline update-home update-returning)

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

(def speed (c/screen-to-world 12))
(def returning-speed (c/screen-to-world 0.6))
(def d-time (/ 1.0 60))
(def bomb-y-min (/ (c/screen-to-world c/game-height) 4.0))
(def default-ticks-first-bomb 30)
(def default-ticks-next-bomb 90)
(def large-size (c/screen-to-world 16))
(def mini-size (c/screen-to-world 10))

(defn create-enemy-entity! [screen ship-texture col]
  (let [pixel-ship (texture ship-texture)]
    (doto (assoc pixel-ship
            :body (create-enemy-body! screen)
            :width large-size :height large-size
            :id :enemy-ship :enemy? true :render-layer 70 :score 100
            :translate-x (- (c/screen-to-world c/ship-mp-xoffset)) :translate-y (- (c/screen-to-world c/ship-mp-yoffset))
            :drift-x-delta (* (c/distance-from-center col) c/drift-x-delta)
            :drift-y-delta (/ (* (* (c/distance-from-center col) (c/distance-from-center col)) c/drift-x-delta) 20.0)
            :movement-state starting-state
            :ticks-to-bomb (rand-int default-ticks-first-bomb)
            :ship-texture ship-texture)
        (body! :set-linear-velocity 0 0))))

(defn create-mini-entity! [screen ship-texture x y]
  (let [mini-ship (assoc (texture ship-texture)
                    :body (create-mini-body! screen)
                    :x x :y y
                    :translate-x (- (c/screen-to-world 4)) :translate-y (- (c/screen-to-world 6.6))
                    :width mini-size :height mini-size
                    :id :mini-enemy :mini? true :render-layer 70 :score 200
                    :ticks-to-bomb (rand-int default-ticks-first-bomb))]
    (doto mini-ship
      (body! :set-linear-velocity 0 (c/screen-to-world -20.0))
      (body-position! x y 180.0))
    mini-ship
    ))

(defn create-enemy-body!
  [screen]
  (let [body (add-body! screen (body-def :dynamic :bullet true))
        enemy-shape (polygon-shape :set-as-box (c/screen-to-world 3) (c/screen-to-world 3) (vector-2 0 0) 0)]
    (->> enemy-shape
         (fixture-def :density 1 :friction 0 :restitution 1 :is-sensor true :shape)
         (body! body :create-fixture))
    (.dispose enemy-shape)
    body))

(defn create-mini-body!
  [screen]
  (let [body (add-body! screen (body-def :dynamic :bullet true))
        enemy-shape (polygon-shape :set-as-box (c/screen-to-world 2) (c/screen-to-world 2) (vector-2 0 0) 0)]
    (->> enemy-shape
         (fixture-def :density 1 :friction 0 :restitution 1 :is-sensor true :shape)
         (body! body :create-fixture))
    (.dispose enemy-shape)
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

(defn move [screen entity]
  (let [ms (:movement-state entity)
        entity (update-home entity screen)]
    (cond (= :drifting ms)
          (update-drift entity screen)
          (= :attacking ms)
          (update-from-spline entity)
          (= :returning ms)
          (update-returning entity screen))
    ))

(defn drop-bomb [screen entity]
  (let [ms (:movement-state entity)]
    (cond (= :attacking ms)
          (let [ttb? (and (<= (:ticks-to-bomb entity) 0)
                          (> (:y entity) bomb-y-min))]
            (if ttb?
              (list (assoc entity :ticks-to-bomb default-ticks-next-bomb) (bomb/create-bomb screen (:x entity) (- (:y entity) (c/screen-to-world 6))))
              (assoc entity :ticks-to-bomb (- (:ticks-to-bomb entity) 1))))
          :else entity
        )))

(defn handle-collision [enemy other-entity screen entities]
  (cond (:bullet? other-entity)
        (if-let [x (:x enemy)]
          (cond (= :drifting (:movement-state enemy));Keep these separate as there will eventually be different behaviorx
                (do
                  (update! screen :p1-level-score (+ (:p1-level-score screen) (:score enemy)))
                  (remove #(or (= enemy %)
                               (= other-entity %))
                          (conj entities (exp/create-explosion (:x enemy) (:y enemy)))))
                (= :attacking (:movement-state enemy))
                (do
                  (update! screen :p1-level-score (+ (:p1-level-score screen) (:score enemy)))
                  (remove #(or (= enemy %)
                               (= other-entity %))
                          (conj entities (exp/create-explosion (:x enemy) (:y enemy)))))
                (= :returning (:movement-state enemy))
                (do
                  (update! screen :p1-level-score (+ (:p1-level-score screen) (:score enemy))
                           :to-do {:f create-mini-entity! :args [screen (:ship-texture enemy) (:x enemy) (:y enemy)]})
                  (remove #(or (= enemy %)
                               (= other-entity %))
                          (conj entities (exp/create-explosion (:x enemy) (:y enemy)))
                          ))
                :else nil
                ))))

(defn handle-attack [{:keys [ticks game-state can-attack?] :as screen} entities]
  (let [attack? (and can-attack?
                     (= game-state :in-game)
                     (or
                       (= (mod ticks c/between-attack-ticks) 0)
                       (= (mod (+ ticks 20) c/between-attack-ticks) 0)))]
    (cond attack? (do
                    ;(prn :attack!)
                    (let [enemies (filter #(:enemy? %) entities)
                          non-enemies (filter #(nil? (:enemy? %)) entities)
                          drifters (shuffle (filter #(= (:movement-state %) :drifting) enemies))
                          non-drifters (filter #(not= (:movement-state %) :drifting) enemies)
                          entity (first drifters)
                          attacker (cond entity
                                         (assoc entity :movement-state (state-machine (:movement-state entity)) :current-time 0
                                           :spline (splines/calibrate-spline (:x entity) (:y entity) (:row entity)))
                                         :else nil)]
                      ;(prn :entities (count entities) :enemies (count enemies) :non-enemies (count non-enemies) :drifters (count drifters) :non-drifters (count non-drifters))
                      (cond (nil? attacker) entities
                            :else (conj (conj (conj (rest drifters) attacker) non-drifters) non-enemies))))
          :else entities)))

(defn update-home [entity screen]
  (let [on-left (< (:home-x entity) c/half-game-width-world)
        outward  (:formation-expand? screen)
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
        l (vector-2! dir-vec :len)]
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
