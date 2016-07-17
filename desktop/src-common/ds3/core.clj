(ns ds3.core
  (:require [play-clj.core :refer :all]
            [ds3.common :as c]
            [ds3.ship :as ship]
            [ds3.main-screen :as main]))


(defgame ds3-game
  :on-create
  (fn [this]
    (set-screen! this main/main-screen)))
