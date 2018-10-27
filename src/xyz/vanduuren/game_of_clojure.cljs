(ns ^:figwheel-hooks xyz.vanduuren.game-of-clojure
  (:require
   [goog.dom :as gdom]))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(def canvas (gdom/getElement "canvas"))
(def ctx (.getContext canvas "2d"))
(def tile-size 10)
(def color-on "black")
(def color-off "white")

(defn tile-color [bit]
  (if (= bit 1)
    color-on
    color-off))

(defn draw-tile! [x y color]
  ;; workaround to get crisp grid lines
  (.setTransform ctx 1, 0, 0, 1, 0.5, 0.5)
  
  ;; begin drawing a new path in the canvas
  (.beginPath ctx)
  (.rect ctx x y tile-size tile-size)

  (set! (.-fillStyle ctx) color)
  (.fill ctx)

  (set! (.-lineWidth ctx) 0.5)
  (set! (.-strokeStyle ctx) color-on)
  (.stroke ctx))

(defn clear-board! []
  (.clearRect ctx 0 0 (.-width ctx) (.-height ctx)))

(defn get-tile [grid x y] (nth (nth grid y) x))

(defn draw-board! [grid]
  ;; base the height and width of the board on the dimensions of grid
  (def height (count grid))
  (def width (if (= height 0) 0 (count (first grid))))

  ;; set the canvas properties accordingly
  (set! (.-height canvas) (+ 1 (* height tile-size)))
  (set! (.-width canvas) (+ 1 (* width tile-size)))

  ;; 
  (mapv
   (fn [y]
     (mapv
      (fn [x]
        (let [tile-state (get-tile grid x y)]
           (draw-tile! (* tile-size x) (* tile-size y) (tile-color tile-state))))
      (range 0 width)))
   (range 0 height)))

(draw-board! [[0 1 0] [0 1 1] [1 1 0] [1 1 1] [0 0 0]])

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
