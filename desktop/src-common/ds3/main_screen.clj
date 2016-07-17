(ns ds3.main-screen
  (:require [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer :all]
            [ds3.common :as c]
            [ds3.ship :as ship])
  (:import [com.badlogic.gdx.physics.box2d Box2DDebugRenderer]))

(declare handle-all-entities move-player-tick move)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false (c/screen-to-world c/game-width) (c/screen-to-world c/game-height))
                          :world (box-2d 0 0);-2.0)
                          :debug-renderer (Box2DDebugRenderer.))
          pixel-ship (ship/create-ship-entity! screen)]
      [pixel-ship]))

  :on-render
  (fn [screen entities]
    (let [debug-renderer (:debug-renderer screen)
          world (:world screen)
          camera (:camera screen)]
      (clear!)
      (let [entities
            (->> entities
                 (step! screen)
                 (handle-all-entities screen)
                 (render! screen))]
        (.render debug-renderer world (.combined camera))
        entities))))


(defn handle-all-entities [screen entities]
  (->> entities
       (map (fn [entity]
              (cond-> entity
                      (:ship? entity) ship/move-player-tick)))
       ))
