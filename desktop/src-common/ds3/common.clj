(ns ds3.common)

(def ^:const s-to-w-divider 50.0)

(defn screen-to-world [x]
  (float (/ x s-to-w-divider)))

(def ^:const game-width 224.0)

(def ^:const game-height 288.0)

(def ^:const starting-rank 0)

(def ^:const game-width-adj (screen-to-world 224.0))

(def ^:const game-height-adj (screen-to-world 288.0))

(def ^:const half-game-width-world (screen-to-world (/ game-width 2)))

(def ^:const ship-mp-xoffset 6)

(def ^:const ship-mp-yoffset 10)

(def ^:const enemy-rows 5)

(def ^:const enemy-columns 10)

(def ^:const center (float (/ (- enemy-columns 1) 2)))

(def ^:const enemy-width-min 16)

(def ^:const enemy-width-max 22)

(def ^:const enemy-width-start enemy-width-min)

(def ^:const drift-ticks 240)

(def ^:const between-attack-ticks 120)

(def ^:const between-attack-delta 5)

(def ^:const between-attack-max-delta 50)

(def ^:const drift-x-delta (screen-to-world (/ (- enemy-width-max enemy-width-min) drift-ticks)))

(def ^:const enemy-height 20)

(def ^:const enemy-start-x (/ (- game-width (* (- enemy-columns 1) enemy-width-start)) 2));(- (/ (- game-width (* (- enemy-columns 1) enemy-width-start)) 2) ship-mp-xoffset))

(def ^:const enemy-start-y 170)

(defn distance-from-center [n]
   (Math/abs (- center n)))

