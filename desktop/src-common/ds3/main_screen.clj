(ns ds3.main-screen
  (:require [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer :all]
            [ds3.common :as c]
            [ds3.ship :as ship]
            [ds3.bullet :as bullet])
  (:import [com.badlogic.gdx.physics.box2d Box2DDebugRenderer]))

(declare handle-all-entities move-player-tick move create-oob-entity! create-oob-body! check-for-input mark-for-removal)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false (c/screen-to-world c/game-width) (c/screen-to-world c/game-height))
                          :world (box-2d 0 0);-2.0)
                          :debug-renderer (Box2DDebugRenderer.))
          top-oob (doto (create-oob-entity! screen (c/screen-to-world c/game-width) (c/screen-to-world 20))
                    (body-position! 0 (c/screen-to-world (- c/game-height 10)) 0))
          pixel-ship (ship/create-ship-entity! screen)]
      [(assoc top-oob :id :top-oob :oob? true)
       ;(for [col (range c/enemy-columns)
       ;      row (range c/enemy-rows)
       ;      :let [x (+ c/enemy-start-x (* col c/enemy-width))
       ;            y (+ c/enemy-start-y (* row c/enemy-height))]]
       ;  (doto (ship/create-enemy-entity! screen (rand-int Integer/MAX_VALUE))
       ;           (body-position! (c/screen-to-world x) (c/screen-to-world y) 0)))
       pixel-ship]))

  :on-render
  (fn [screen entities]
    (let [debug-renderer (:debug-renderer screen)
          world (:world screen)
          camera (:camera screen)]
      (clear!)
      (let [entities
            (->> entities
                 (step! screen)
                 (check-for-input screen)
                 (handle-all-entities screen)
                 (render! screen))]
        (.render debug-renderer world (.combined camera))
        entities)))

  :on-begin-contact
  (fn [screen entities]
    (let [entity (first-entity screen entities)
          entity2 (second-entity screen entities)
          world (:world screen)]
      ;(prn :body-count (box-2d! screen :get-body-count))
      (cond
        (or (:bullet? entity)
            (:bullet? entity2))
        (do
          ;(prn (:id entity) (:id entity2))
          (if (:bullet? entity)
            (mark-for-removal entity entities))
          (if (:bullet? entity2)
            (mark-for-removal entity2 entities)))
        ;:else entities
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
  (->> entities
       (map (fn [entity]
              (cond-> entity
                      (:ship? entity) ship/move-player-tick)))
       ))

(defn check-for-input [screen entities]
   (cond
     (and (get screen :fire-when-ready true)
          (key-pressed? :x)) (let [ship (first (filter #(:ship? %) entities))
                               x (+ (:x ship) (c/screen-to-world c/ship-mp-xoffset))
                               y (+ (:y ship) (c/screen-to-world c/ship-mp-yoffset))]
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

(defn mark-for-removal [entity entities]
  (do
    (remove #(= entity %) entities)
    ))

(-> main-screen :entities deref)

;(do (use 'ds3.core.desktop-launcher)(-main))
