(ns ds3.common)


(def ^:const s-to-w-divider 50.0)

(def ^:const game-width 224.0)

(def ^:const game-height 288.0)

(defn screen-to-world [x]
  (float (/ x s-to-w-divider)))
