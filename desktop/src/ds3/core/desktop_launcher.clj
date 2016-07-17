(ns ds3.core.desktop-launcher
  (:require [ds3.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. ds3-game "ds3"  (* 3 224) (* 3 288))
  (Keyboard/enableRepeatEvents true))
