(ns ds3.main-screen
  (:require [play-clj.core :refer [add-timer! bundle clear! defscreen game key-code key-pressed? orthographic render! screen! stage update!]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! box-2d chain-shape first-entity fixture-def second-entity step!]]
            [ds3.common :as c]
            [ds3.bomb :as bomb]
            [ds3.ship :as ship]
            [ds3.enemy :as enemy]
            [ds3.bullet :as bullet]
            [ds3.explosion :as exp]
            [ds3.hud :as hud]
            [ds3.space :as sp])

  (:import [com.badlogic.gdx.physics.box2d Box2DDebugRenderer]))

(declare handle-all-entities create-oob-entity! create-oob-body! check-for-input mark-for-removal)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false (c/screen-to-world c/game-width) (c/screen-to-world c/game-height))
                          :world (box-2d 0 0);-2.0)
                          :ticks 0
                          :level-score 0
                          :formation-expand false
                          :debug-renderer (Box2DDebugRenderer.))
          top-oob (doto (create-oob-entity! screen (c/screen-to-world c/game-width) (c/screen-to-world 20))
                    (body-position! 0 (c/screen-to-world (- c/game-height 10)) 0))
          pixel-ship (ship/create-ship-entity! screen)
          space (sp/create-space)]
      [(assoc top-oob :id :top-oob :oob? true :render-layer 0)
       (enemy/create-enemies screen)
       space
       pixel-ship]))

  :on-render
  (fn [screen entities]
    (update! screen :ticks (inc (:ticks screen)))
    (let [debug-renderer (:debug-renderer screen)
          world (:world screen)
          camera (:camera screen)]
      (clear! 0.1 0.1 0.12 1)
      (cond (= (mod (:ticks screen) c/drift-ticks) 0)
            (update! screen :formation-expand (not (:formation-expand screen))))
      (let [entities
            (->> entities
                 (step! screen)
                 (check-for-input screen)
                 (handle-all-entities screen)
                 ;(bomb/animate-bomb screen)
                 (sort-by :render-layer)
                 (render! screen))]
        ;(.render debug-renderer world (.combined camera))
        entities)))

  :on-begin-contact
  (fn [screen entities]
    (let [entity (first-entity screen entities)
          entity2 (second-entity screen entities)]
      ;(prn :entity (:id entity) :entity2 (:id entity2))
      (cond
        (:enemy? entity) (enemy/handle-collision entity entity2 screen entities)
        (:enemy? entity2) (enemy/handle-collision entity2 entity screen entities)
      )))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :refresh-shot (update! screen :fire-when-ready true)
      nil)
    nil)

  :on-key-up
  (fn [screen entities]
    (cond (= (:key screen) (key-code :x))
          (update! screen :fire-when-ready true))
    entities)
  )


(defn handle-all-entities [screen entities]
  (let [entities
        (->> entities
             (map (fn [entity]
                    (cond (:ship? entity) (ship/move-player-tick entity)
                          (:enemy? entity) (-> entity
                                               (enemy/move screen)
                                               (enemy/drop-bomb screen))
                          (:explosion? entity) (exp/handle-explosion entity)
                          (:bomb? entity) (bomb/animate-bomb screen entity)
                          (:star? entity) (sp/move-star screen entity)
                          :else entity)))
             )]
    entities
    ))

(defn check-for-input [screen entities]
   (cond
     (and (get screen :fire-when-ready true)
          (key-pressed? :x)) (let [ship (first (filter #(:ship? %) entities))
                               x (:x ship)
                               y (:y ship)]
                               ;(prn :x x :y y)
                         (update! screen :fire-when-ready false)
                         (add-timer! screen :refresh-shot 0.2)
                         (conj entities (bullet/create-bullet! screen x (+ 0.1 y))))
     :else entities
     ))

(defn create-oob-entity!
  [screen width height]
  (let [rect (bundle nil)]
  (assoc rect
         :body (create-oob-body! screen width height)
         :width width :height height)))

(defn create-oob-body!
  [screen width height]
  (let [body (add-body! screen (body-def :static))]
    (->> [0 0
          0 height
          width height
          width 0
          0 0]
         float-array
         (chain-shape :create-chain)
         (fixture-def
           :is-sensor true
           :density 1 :restitution 1 :shape )
         (body! body :create-fixture))
body))

(-> main-screen :entities deref)

;(do (use 'ds3.core.desktop-launcher)(-main))
