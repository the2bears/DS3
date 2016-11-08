(ns ds3.sounds
  (:require [play-clj.core :refer [music music! sound sound!]]))

(def sounds (atom {}))

(defn init-sounds[]
  (do
    (swap! sounds assoc :bullet (music "shot3.ogg" :set-looping false))
    (swap! sounds assoc :explosion (music "explosion.ogg" :set-looping false))
    (swap! sounds assoc :bomb (music "bomb2.ogg" :set-looping false))))


(defn play-once
  ([sound-type]
   (play-once sound-type 1.0))
  ([sound-type volume]
   (let [sound (sound-type @sounds)]
     (if-not
       (music! sound :is-playing)
       (doto sound
         (music! :set-volume volume)
         (music! :play))))))
