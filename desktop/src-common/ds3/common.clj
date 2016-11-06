(ns ds3.common
  (:require [pixel-ships.bollinger :as bollinger :refer [color-scheme]]))

(def debug false)

(def ^:const s-to-w-divider 50.0)

(defn screen-to-world [x]
  (float (/ x s-to-w-divider)))

(def ^:const game-width 224.0)

(def ^:const game-height 288.0)

(def ^:const starting-rank 0)

(def ^:const game-width-adj (screen-to-world game-width))

(def ^:const game-height-adj (screen-to-world game-height))

(def ^:const half-game-width-world (screen-to-world (/ game-width 2)))

(def ^:const half-game-height-world (screen-to-world (/ game-height 2)))

(def ^:const ship-mp-xoffset 6)

(def ^:const ship-mp-yoffset 10)

(def ^:const ship-y-default (screen-to-world (/ game-height 15)))

(def ^:const enemy-rows 5);5

(def ^:const enemy-columns 10);10

(def ^:const center (float (/ (- enemy-columns 1) 2)))

(def ^:const enemy-width-min 16)

(def ^:const enemy-width-max 22)

(def ^:const enemy-width-start enemy-width-min)

(def ^:const drift-ticks 240)

(def ^:const between-attack-ticks 120)

(def ^:const between-attack-delta 4)

(def ^:const between-attack-max-delta 50)

(def ^:const drift-x-delta (screen-to-world (/ (- enemy-width-max enemy-width-min) drift-ticks)))

(def ^:const enemy-height 20)

(def ^:const capture-height 14)

(def ^:const capture-height-shifted -14)

(def ^:const enemy-start-x (/ (- game-width (* (- enemy-columns 1) enemy-width-start)) 2));(- (/ (- game-width (* (- enemy-columns 1) enemy-width-start)) 2) ship-mp-xoffset))

(def ^:const enemy-start-y 170)

(def ^:const beaming-ticks 180)

(def ^:const ghost-dropping-speed (screen-to-world -60.0))

(def ghost-color (assoc bollinger/color-scheme :sat-mid 0.12 :sat-delta 0.05))

(defn distance-from-center [n]
   (Math/abs (- center n)))

(defn rand-keyword [] (keyword (str (java.util.UUID/randomUUID))))
