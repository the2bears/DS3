(ns ds3.spark
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format update! sound x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [vector-2 vector-2!]]
            [ds3.common :as c]))

(declare random-ticks random-velocity remove-spark-emitter internal-remove-spark-emitter)

(def spark-textures (atom []))
(def spark-emitters (atom []))
(def removed-emitters (atom {}))
(def spark-speed-slow (c/screen-to-world -0.3))
(def blues [(color :black)(color :blue)(color :cyan)(color :white)])

(defn- create-spark-texture [c]
  (let [pix-map (pixmap* 1 1 (pixmap-format :r-g-b-a8888))]
    (doto pix-map
      (pixmap! :set-color c)
      (pixmap! :fill-rectangle 0 0 1 1))
    (texture pix-map)))

(defn create-spark [x y spark-texture]
  (assoc (texture spark-texture)
    :width (c/screen-to-world 1) :height (c/screen-to-world 1)
    :x x :y y
    :spark? true :id :spark
    :render-layer 72;2
    :velocity (random-velocity)
    :ttl (random-ticks)
    ))

(defn- random-velocity []
  (vector-2 0 spark-speed-slow :rotate (rand-int 360)))

(defn- random-ticks []
  (+ 25 (rand-int 45)))

(defn init-spark [screen]
  (do
    (reset! spark-textures (map (fn [c] (create-spark-texture (color c))) blues))))

(defn create-spark-emitter
  ([{:keys [:x :y] :as enemy}]
   (create-spark-emitter enemy (c/rand-keyword)))
  ([{:keys [:x :y] :as enemy} emitter-id]
   (let [emitter {:x x :y y :id emitter-id :enemy-id (:id enemy)}]
     (swap! spark-emitters conj emitter)
     emitter-id)))

(defn update-emitter [spark-emitter-id entity]
  (let [removed-emitters @removed-emitters]
    (cond (contains? removed-emitters spark-emitter-id)
          spark-emitter-id
          :else
          (do
            (internal-remove-spark-emitter spark-emitter-id)
            (create-spark-emitter entity spark-emitter-id)))))

(defn- internal-remove-spark-emitter [emitter]
  (let [new-emitters (filter #(not= emitter (:id %)) @spark-emitters)]
    (reset! spark-emitters new-emitters)
    ))

(defn remove-spark-emitter [emitter]
  (let [new-emitters (filter #(not= emitter (:id %)) @spark-emitters)
        new-removed (assoc @removed-emitters emitter true)]
    (reset! spark-emitters new-emitters)
    (reset! removed-emitters new-removed)
    ))

(defn update-spark [screen {:keys [:ttl :velocity] :as entity}]
  (cond (> ttl 0) (assoc entity :x (+ (:x entity) (x velocity)) :y (+ (:y entity) (y velocity)) :ttl (- ttl 1))
        :else nil))

(defn handle-sparks [{:keys [:ticks] :as screen} entities]
  (cond (= (mod ticks 3) 0)
        (reduce #(conj %1 (create-spark (:x %2) (:y %2) (first (shuffle @spark-textures)))) entities @spark-emitters)
        :else entities))
