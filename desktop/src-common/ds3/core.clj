(ns ds3.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer [bitmap-font bitmap-font!]]
            [play-clj.ui :refer [label label! style]]
            [ds3.common :as c]
            [ds3.ship :as ship]
            [ds3.main-screen :as main]
            [ds3.fps :as fps]
            [ds3.hud :as hud])
  (:import [com.badlogic.gdx.graphics.g2d Batch]))

(declare ds3-controller ds3-game)

(def ^:const game-over-x 246.0)
(def ^:const game-over-y 420.0)

(defgame ds3-game
  :on-create
  (fn [this]
    (set-screen! this ds3-controller main/main-screen hud/hud-screen)
    (graphics! :set-v-sync true)))

(defscreen ds3-controller
  :on-show
  (fn [screen entities]
    (update! screen :fps? false)
    entities)

  :on-render
  (fn [screen entities]
    entities)

  :on-key-up
  (fn [screen entities]
    (cond (= (:key screen) (key-code :f))
          (do
            (let [fps? (not (:fps? screen))]
              (prn :fps? fps?)
              (update! screen :fps? fps?)
              (cond fps?
                    (on-gl (set-screen! ds3-game ds3-controller main/main-screen fps/fps-screen hud/hud-screen))
                    :else (on-gl (set-screen! ds3-game ds3-controller main/main-screen hud/hud-screen))
                    ))))
    entities))
