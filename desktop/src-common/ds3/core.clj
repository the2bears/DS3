(ns ds3.core
  (:require [play-clj.core :refer :all]
            [ds3.common :as c]
            [ds3.ship :as ship]
            [ds3.main-screen :as main]
            [ds3.fps :as fps]
            [ds3.hud :as hud]))


(defgame ds3-game
  :on-create
  (fn [this]
    (set-screen! this main/main-screen fps/fps-screen hud/hud-screen)
    (graphics! :set-v-sync true)))
