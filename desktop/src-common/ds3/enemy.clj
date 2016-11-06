(ns ds3.enemy
  (:require [ds3.beam :as beam]
            [ds3.bomb :as bomb]
            [ds3.common :as c]
            [ds3.explosion :as exp]
            [ds3.ship :as ship]
            [ds3.spark :as spark]
            [ds3.splines :as splines]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [play-clj.core :refer [add-timer! bundle shape color key-pressed? pixmap! pixmap* screen! sound update! x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer [ b-spline b-spline! vector-2 vector-2!]])
  (:import [com.badlogic.gdx.graphics Pixmap Texture TextureData Pixmap$Format]))

(declare convert-ghost create-enemy-body! create-mini-body! create-minis explode-enemy rand-keyword
         spark-enemy update-beaming update-drift update-from-spline update-dropping
         update-emitter update-home update-returning update-towing update-turning)

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

(def state-machine {:drifting :attacking :attacking :returning :returning :drifting
                    :capturing :beaming :beaming :dropping :dropping :returning
                    :turning :towing :towing :drifting})

(def starting-state :drifting)

(def speed (c/screen-to-world 12))
(def returning-speed (c/screen-to-world 0.6))
(def dropping-speed (- (c/screen-to-world 0.7)))
(def towing-speed (c/screen-to-world 1.0))
(def starting-angle 0)
(def rotating-speed 8)
(def d-time (/ 1.0 60))
(def bomb-y-min (/ (c/screen-to-world c/game-height) 4.0))
(def default-ticks-first-bomb 30)
(def default-ticks-next-bomb 90)
(def ticks-next-bomb-rank-delta 4)
(def ticks-next-bomb-max-delta 60)
(def large-size (c/screen-to-world 16))
(def mini-size (c/screen-to-world 10))
(def mini-ticks-per 3)
(def mini-wiggle-angles [180 175 170 165 170 175 180 185 190 195 190 185])


(defn- create-enemy-entity! [screen ship-texture col]
  (let [pixel-ship (texture ship-texture)]
    (doto (assoc pixel-ship
            :body (create-enemy-body! screen)
            :width large-size :height large-size
            :id :enemy :enemy? true :render-layer 70 :score 100
            :translate-x (- (c/screen-to-world c/ship-mp-xoffset)) :translate-y (- (c/screen-to-world c/ship-mp-yoffset))
            :drift-x-delta (* (c/distance-from-center col) c/drift-x-delta)
            :drift-y-delta (/ (* (* (c/distance-from-center col) (c/distance-from-center col)) c/drift-x-delta) 20.0)
            :movement-state starting-state
            :ticks-to-bomb (rand-int default-ticks-first-bomb)
            :ship-texture ship-texture)
      (body! :set-linear-velocity 0 0))))

(defn- create-mini-entity! [screen ship-texture x y bonus-group]
  (let [mini-ship (assoc (texture ship-texture)
                    :body (create-mini-body! screen)
                    :x x :y y
                    :translate-x (- (c/screen-to-world 4)) :translate-y (- (c/screen-to-world 6.6))
                    :width mini-size :height mini-size
                    :id :mini-enemy :mini? true :render-layer 70 :score 200
                    :movement-state :falling
                    :bonus-group bonus-group
                    :ticks-to-bomb (rand-int default-ticks-first-bomb)
                    :ticks-to-wiggle 0)]
    (doto mini-ship
      (body! :set-linear-velocity 0 (c/screen-to-world -65.0))
      (body-position! x y 180.0))
    mini-ship
    ))

(defn- create-enemy-body!
  [screen]
  (let [body (add-body! screen (body-def :dynamic :bullet true))
        enemy-shape (polygon-shape :set-as-box (c/screen-to-world 3) (c/screen-to-world 3) (vector-2 0 0) 0)]
    (->> enemy-shape
         (fixture-def :density 1 :friction 0 :restitution 1 :is-sensor true :shape)
         (body! body :create-fixture))
    (.dispose enemy-shape)
    body))

(defn- create-mini-body!
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
      (doto (assoc (create-enemy-entity! screen (nth ship-textures row) col)
              :hp (if (= row (- c/enemy-rows 1)) 2 1)
              :boss? (if (= row (- c/enemy-rows 1)) true false)
              :score (if (= row (- c/enemy-rows 1)) 400 200)
              :row row :col col :home-x (c/screen-to-world x) :home-y (c/screen-to-world y))
        (body-position! (c/screen-to-world x) (c/screen-to-world y) starting-angle))
      )))

(defn create-minis [screen ship-texture x y]
  (let [bonus-group (c/rand-keyword)]
    (update! screen bonus-group 4)
    (doseq [xx (range 4)]
      (let [next-keyword (c/rand-keyword)]
        (update! screen next-keyword {:f create-mini-entity! :args [screen ship-texture x y bonus-group]})
        (add-timer! screen next-keyword (* 0.15 xx)))))
  )

(defn move [screen entities entity]
  (let [ms (:movement-state entity)
        entity (update-home screen entity)];(update-emitter entity screen)
    (update-emitter screen entity)
    (cond (= :drifting ms)
          (update-drift screen entity)
          (= :attacking ms)
          (update-from-spline screen entity)
          (= :capturing ms)
          (update-from-spline screen entity)
          (= :returning ms)
          (update-returning screen entity)
          (= :dropping ms)
          (update-dropping screen entity)
          (= :beaming ms)
          (update-beaming screen entities entity)
          (= :turning ms)
          (update-turning screen entity)
          (= :towing ms)
          (update-towing screen entity)
          :else
          entity)))

(defn update-emitter [screen entity]
  (if (some? (:spark-emitter entity))
    (spark/update-emitter (:spark-emitter entity) entity))
  entity)

(defn drop-bomb [screen entity]
  (let [ms (:movement-state entity)
        rank (:p1-rank screen)]
    (cond (= :attacking ms)
          (let [ttb? (and (<= (:ticks-to-bomb entity) 0)
                          (> (:y entity) bomb-y-min))]
            (if ttb?
              (list (assoc entity :ticks-to-bomb (- default-ticks-next-bomb (min ticks-next-bomb-max-delta (* rank ticks-next-bomb-rank-delta))))
                    (bomb/create-bomb screen (:x entity) (- (:y entity) (c/screen-to-world 6))))
              (assoc entity :ticks-to-bomb (- (:ticks-to-bomb entity) 1))))
          :else entity
          )))

(defn handle-collision [enemy other-entity screen entities]
  (cond (:bullet? other-entity)
        (if-let [x (:x enemy)]
          (let [hp (- (:hp enemy) 1)
                m-s (:movement-state enemy)]
            (cond (= :drifting m-s);Keep these separate as there will eventually be different behaviors
                  (if (= hp 0)
                    (explode-enemy enemy other-entity screen entities)
                    (spark-enemy enemy other-entity screen entities))
                  (or (= :capturing m-s) (= :dropping m-s) (= :beaming m-s) (= :attacking m-s))
                  (if (= hp 0)
                    (explode-enemy enemy other-entity screen entities)
                    (spark-enemy enemy other-entity screen entities))
                  (= :returning m-s)
                  (do
                    (if (not (or (:boss? enemy) (= :ghost-ship (:id enemy))))
                      (create-minis screen (:ship-texture enemy) x (:y enemy)))
                    (if (= hp 0)
                      (explode-enemy enemy other-entity screen entities)
                      (spark-enemy enemy other-entity screen entities)))
                  :else nil
                  )))))

(defn- explode-enemy [enemy other-entity screen entities]
  (let [spark-emitter-id (:spark-emitter enemy)]
    (update! screen :p1-level-score (+ (:p1-level-score screen) (* (:score enemy) (:p1-bonus screen))))
    (if (some? spark-emitter-id)
      (spark/remove-spark-emitter spark-emitter-id))
    (cond (= (:movement-state enemy) :beaming)
          (let [ship (first (filter #(:ship? %) entities))
                all-others (filter #(nil? (:ship? %)) entities)
                new-entities (conj all-others (assoc ship :captured? false))]
            (remove #(or (= enemy %)
                         (= other-entity %)
                         (:beam? %))
                    (conj new-entities (exp/create-explosion (:x enemy) (:y enemy)))))
          (:master? enemy)
          (let [ghost (first (filter #(:ghost? %) entities))
                converted-ghost (convert-ghost ghost enemy)]
            (-> (remove #(or (= enemy %)
                             (= other-entity %)
                             (= ghost %)) entities)
                (conj (exp/create-explosion (:x enemy) (:y enemy)))
                (conj converted-ghost)))
          :else
          (remove #(or (= enemy %)
                       (= other-entity %))
                  (conj entities (exp/create-explosion (:x enemy) (:y enemy)))))))

(defn- spark-enemy [{:keys [:hp] :as enemy} other-entity screen entities]
  (let [spark-emitter-id (spark/create-spark-emitter enemy)]
    (sound "explosion.ogg" :play 0.35)
    (remove #(= other-entity %) (map #(if
                                        (= enemy %1)
                                        (assoc %1 :hp (- hp 1) :spark-emitter spark-emitter-id)
                                        %1)
                                     entities))))

(defn- convert-ghost [ghost {:keys [:movement-state] :as master}]
  (if (= :attacking movement-state)
    (do
      (body! ghost :set-linear-velocity 0 c/ghost-dropping-speed)
      ghost)
    (assoc ghost :ghost? false :enemy? true
      :drift-x-delta (:drift-x-delta master)
      :drift-y-delta (:drift-y-delta master)
      :row (:row master) :col (:col master) :home-x (:home-x master) :home-y (:home-y master)
      :movement-state :returning :hp 1 :score 500
      :ticks-to-bomb (rand-int default-ticks-first-bomb)
      )))

(defn handle-mini-collision [mini other-entity screen entities]
  (cond (:bullet? other-entity)
        (if-let [x (:x mini)]
          (do
            (let [bonus-group (:bonus-group mini)
                  bonus-count (- (bonus-group screen) 1)]
              (if (= 0 bonus-count) (update! screen
                                             :p1-bonus (+ 1 (:p1-bonus screen))
                                             :p1-rank (+ 1 (:p1-rank screen))))
              (update! screen :p1-level-score (+ (:p1-level-score screen) (* (:score mini) (:p1-bonus screen))) bonus-group bonus-count)
              (remove #(or (= mini %)
                           (= other-entity %))
                      (conj entities (exp/create-explosion x (:y mini)))))))
        (:oob? other-entity)
        (remove #(= mini %) entities)
        ))

(defn handle-attack [{:keys [ticks game-state can-attack?] :as screen} entities]
  (let [rank (:p1-rank screen)
        b-a-t (- c/between-attack-ticks (min c/between-attack-max-delta (* rank c/between-attack-delta)))
        attack? (and can-attack?
                     (= game-state :in-game)
                     (or
                       (= (mod ticks b-a-t) 0)
                       (and
                         (> rank 2)
                         (= (mod (+ ticks 15) b-a-t) 0))
                       (and
                         (> rank 7)
                         (= (mod (+ ticks 30) b-a-t) 0))))]
    (cond attack? (do
                    ;(prn :attack!)
                    (let [enemies (filter #(:enemy? %) entities)
                          non-enemies (filter #(nil? (:enemy? %)) entities)
                          drifters (shuffle (filter #(= (:movement-state %) :drifting) enemies))
                          non-drifters (filter #(not= (:movement-state %) :drifting) enemies)
                          capturers (filter #(= (:movement-state %) :capturing) non-drifters)
                          beamers (filter #(= (:movement-state %) :beaming) non-drifters)
                          master (filter #(:master? %) enemies)
                          ghost (filter #(:ghost? %) entities)
                          doppel (filter #(:doppel? %) entities)
                          entity (first drifters)
                          attacker (cond entity
                                         (if (and (:boss? entity) (empty? capturers) (empty? beamers) (empty? master) (empty? ghost) (empty? doppel))
                                           (let [n-m-s :capturing]
                                             (assoc entity :movement-state n-m-s :current-time 0
                                               :spline (splines/calibrate-spline (assoc entity :movement-state n-m-s))))
                                           (let [n-m-s (state-machine (:movement-state entity))]
                                             (assoc entity :movement-state n-m-s :current-time 0
                                               :spline (splines/calibrate-spline (assoc entity :movement-state n-m-s)))))
                                         :else nil)]
                      ;(prn :entities (count entities) :enemies (count enemies) :non-enemies (count non-enemies) :drifters (count drifters) :non-drifters (count non-drifters))
                      (cond (nil? attacker) entities
                            :else (-> (rest drifters)
                                      (conj attacker)
                                      (conj non-drifters)
                                      (conj non-enemies)))))
          :else entities)))

(defn- update-home [screen entity]
  (let [on-left (< (:home-x entity) c/half-game-width-world)
        outward  (:formation-expand? screen)
        b (cond on-left outward
                :else (not outward))
        delta-x-fn (cond b -
                         :else +)
        delta-y-fn (cond outward -
                         :else +)]
    (assoc entity :home-x (delta-x-fn (:home-x entity) (:drift-x-delta entity)) :home-y (delta-y-fn (:home-y entity) (:drift-y-delta entity)))))

(defn- update-drift [screen entity]
  (body-position! entity (:home-x entity) (:home-y entity) (:angle entity))
  entity)

(defn- update-from-spline [screen entity]
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
            (if (not= :capturing (:movement-state entity))
              (do
                (body-position! entity (:home-x entity) (c/screen-to-world c/game-height) 0)
                (assoc entity :movement-state (state-machine (:movement-state entity))))
              (do
                (list
                  (beam/create-beam screen (:x entity) (:y entity))
                  (assoc entity :beaming-ticks c/beaming-ticks :movement-state (state-machine (:movement-state entity)))))))
          :else
          (do
            (body-position! entity x y a)
            (assoc entity :current-time (+ current-time new-delta)))
          )))

(defn- update-returning
  ([screen entity]
   (update-returning screen entity returning-speed))
  ([screen {:keys [:home-x :home-y] :as entity} speed]
   (let [cur-pos (vector-2 (:x entity) (:y entity))
         target-pos (vector-2 home-x home-y)
         dir-vec (vector-2! target-pos :sub cur-pos)
         l (vector-2! dir-vec :len)]
     (vector-2! dir-vec :set-length speed)
     (cond (< l returning-speed)
           (do
             (body-position! entity home-x home-y 0)
             (assoc entity :movement-state (state-machine (:movement-state entity))))
           :else
           (do
             (body-position! entity (+ (:x entity) (x dir-vec)) (+ (:y entity) (y dir-vec)) 0)
             entity)))))

(defn- update-dropping [screen {:keys [:x :y] :as entity}]
  (cond (> y (- (c/screen-to-world 10)))
        (do
          (body-position! entity x (+ y dropping-speed) 180)
          entity)
        :else
        (do
          (body-position! entity (:home-x entity) (c/screen-to-world c/game-height) 0)
          (assoc entity :movement-state (state-machine (:movement-state entity))))))

(defn- update-beaming [screen entities {:keys [:x :y :beaming-ticks] :as entity}]
  (let [ship (first (filter #(:ship? %) entities))]
    (cond (or (> beaming-ticks 0) (:captured? ship))
          (assoc entity :beaming-ticks (- beaming-ticks 1))
          :else
          (assoc entity :movement-state (state-machine (:movement-state entity))))))

(defn- update-turning [screen {:keys [:x :y :angle] :as entity}]
  (let [angle-diff (- angle starting-angle)
        done-turning (< angle-diff rotating-speed)
        new-angle (cond done-turning starting-angle
                        :else (- angle rotating-speed))]
    (body-position! entity x y new-angle)
    (assoc entity :movement-state (cond done-turning (state-machine (:movement-state entity))
                                        :else (:movement-state entity)))))

(defn- update-towing [screen entity]
  (update-returning screen entity towing-speed))

(defn handle-mini [{:keys [:ticks-to-wiggle] :as entity}]
  (let [mini-nth (mod (quot ticks-to-wiggle mini-ticks-per) (count mini-wiggle-angles))]
    (assoc entity :ticks-to-wiggle (inc ticks-to-wiggle) :angle (nth mini-wiggle-angles mini-nth))))
