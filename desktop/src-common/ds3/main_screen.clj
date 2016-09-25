(ns ds3.main-screen
  (:require [play-clj.core :refer [add-timer! bundle clear! defgame defscreen game graphics! key-code key-pressed? orthographic render! screen! set-screen! stage update!]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! box-2d chain-shape first-entity fixture-def second-entity step!]]
            [ds3.common :as c]
            [ds3.bomb :as bomb]
            [ds3.ship :as ship]
            [ds3.enemy :as enemy]
            [ds3.bullet :as bullet]
            [ds3.explosion :as exp]
            [ds3.fps :as fps]
            [ds3.hud :as hud]
            [ds3.space :as sp])

  (:import [com.badlogic.gdx.physics.box2d Box2DDebugRenderer]))

(declare check-for-input check-game-status create-oob-entity! create-oob-body! handle-all-entities main-screen on-new-game)

(defgame ds3-game
  :on-create
  (fn [this]
    (set-screen! this main-screen hud/hud-screen fps/fps-screen)
    (graphics! :set-v-sync true)))

;game-state values can be :attract-mode :in-game :game-over and ???

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false (c/screen-to-world c/game-width) (c/screen-to-world c/game-height))
                          :world (box-2d 0 0);-2.0)
                          :ticks 0
                          :p1-level-score 0
                          :high-score 0
                          :p1-lives 0;3
                          :game-state :attract-mode
                          :formation-expand? false
                          :wave-respawning? false
                          :can-attack? false
                          :debug-renderer (Box2DDebugRenderer.))
          top-oob (doto (create-oob-entity! screen (c/screen-to-world c/game-width) (c/screen-to-world 20))
                    (body-position! 0 (c/screen-to-world c/game-height) 0))
          bottom-oob (doto (create-oob-entity! screen (c/screen-to-world (+ c/game-width 20)) (c/screen-to-world 20))
                    (body-position! (c/screen-to-world (- 10)) (c/screen-to-world (- 20)) 0))
          space (sp/create-space)]
      [(assoc top-oob :id :top-oob :oob? true :render-layer 0)
       (assoc bottom-oob :id :bottom-oob :oob? true :render-layer 0)
       space]))

  :on-render
  (fn [screen entities]
    (update! screen :ticks (inc (:ticks screen)))
    (let [debug-renderer (:debug-renderer screen)
          world (:world screen)
          camera (:camera screen)
          ticks (:ticks screen)]
      (clear! 0.1 0.1 0.12 1)
      (cond (= (mod ticks c/drift-ticks) 0)
            (update! screen :formation-expand? (not (:formation-expand? screen))))
      (let [entities
            (->> entities
                 (step! screen)
                 (check-for-input screen)
                 (handle-all-entities screen)
                 (flatten);This is here because when an enemy shoots for one frame it's not a map where :enemy? is true
                 (check-game-status screen)
                 (enemy/handle-attack screen)
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
        (:ship? entity) (ship/handle-collision entity entity2 screen entities)
        (:ship? entity2) (ship/handle-collision entity2 entity screen entities)
        (:bullet? entity) (bullet/handle-collision entity entity2 screen entities)
        (:bullet? entity2) (bullet/handle-collision entity2 entity screen entities)
        (:bomb? entity) (bomb/handle-collision entity entity2 screen entities)
        (:bomb? entity2) (bomb/handle-collision entity2 entity screen entities)
      )))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :refresh-shot (do
                      (update! screen :fire-when-ready true)
                      entities)
      :spawn-wave (do
                    (prn :spawn-wave)
                    (update! screen :wave-respawning? false :formation-expand? false :ticks 0 :can-attack? true)
                    (conj entities (enemy/create-enemies screen)))
      :post-game-over (do
                        (update! screen :p1-level-score 0
                                 :game-state :attract-mode)
                        entities)
      ;default
      nil))

  :on-pause
  (fn [screen entities]
    (do
      (prn :on-pause)
      entities))

  :on-key-up
  (fn [screen entities]
    (case (:game-state screen)
      :in-game
      (cond (= (:key screen) (key-code :x))
            (do
              (update! screen :fire-when-ready true)
              entities))
      :attract-mode
      (cond (= (:key screen) (key-code :num-1))
            (on-new-game screen entities))
      ;default
      entities)
    ))

(defn on-new-game [screen entities]
  (update! screen
           :ticks 0
           :p1-level-score 0
           :p1-lives 3
           :game-state :in-game
           :formation-expand? false
           :wave-respawning? false
           :can-attack? false)
  (prn :on-new-game)
  (remove #(:enemy? %) (conj entities (ship/create-ship-entity! screen))))

(defn handle-all-entities [screen entities]
  (->> entities
       (map (fn [entity]
              (cond (:ship? entity) (ship/move-player-tick screen entity)
                    (:enemy? entity) (-> entity
                                         (enemy/move screen)
                                         (enemy/drop-bomb screen));thread this last, as it might return a bomb along with the enemy
                    (:explosion? entity) (exp/handle-explosion entity)
                    (:bomb? entity) (bomb/animate-bomb screen entity)
                    (:star? entity) (sp/move-star screen entity)
                    :else entity)))
       (ship/collide-with-enemy? screen)
       ))

(defn check-game-status [screen entities]
  (let [ship (first (filter #(:ship? %) entities))
        enemies (filter #(:enemy? %) entities)
        lives (:p1-lives screen)
        p1-score (:p1-level-score screen)
        high-score (if (< (:high-score screen) p1-score) p1-score (:high-score screen))]
    (update! screen :high-score high-score)
    (screen! hud/hud-screen :on-update-lives :p1-lives lives)
    (screen! hud/hud-screen :on-update-score :p1-score (:p1-level-score screen) :high-score high-score)
    (screen! hud/hud-screen :on-update-game-state :game-state (:game-state screen))
    (cond (and (nil? ship)
               (= :in-game (:game-state screen))
               (every? #(= (:movement-state %) :drifting) enemies)
               (> lives 0));lives hasn't had one subtracted yet
          (do
            (let [lives (- lives 1)]
              (update! screen :p1-lives lives)
              (cond (= 0 lives) (do
                                  (update! screen :game-state :game-over)
                                  (add-timer! screen :post-game-over 5)
                                  entities)
                    :else (do
                            (update! screen :can-attack? true)
                            (conj entities (ship/create-ship-entity! screen))))))
          (and (empty? enemies)
               (not (:wave-respawning? screen))
               (= :in-game (:game-state screen)))
          (do
            (add-timer! screen :spawn-wave 3)
            (update! screen :wave-respawning? true)
            entities)
          :else entities)
    ))

(defn check-for-input [screen entities]
  (case (:game-state screen)
    :in-game
    (cond
      (and (get screen :fire-when-ready true)
           (key-pressed? :x))
      (if-let [ship (first (filter #(:ship? %) entities))]
        (let [x (:x ship)
              y (:y ship)]
          ;(prn :x x :y y)
          (update! screen :fire-when-ready false)
          (add-timer! screen :refresh-shot 0.2)
          (conj entities (bullet/create-bullet! screen x (+ 0.1 y))))
        entities)
      :else entities
      )
    ;default
    entities))

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
