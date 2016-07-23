(ns ds3.common)


(def ^:const s-to-w-divider 50.0)

(def ^:const game-width 224.0)

(def ^:const game-height 288.0)

(def ^:const ship-mp-xoffset 6)

(def ^:const ship-mp-yoffset 10)

(def ^:const enemy-rows 5)

(def ^:const enemy-columns 10)

(def ^:const enemy-width 20)

(def ^:const enemy-height 20)

(def ^:const enemy-start-x 12)

(def ^:const enemy-start-y 150)

(defn screen-to-world [x]
  (float (/ x s-to-w-divider)))
