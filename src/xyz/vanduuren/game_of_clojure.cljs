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
(def grid-height 40)
(def grid-width 100)

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
(defn get-dimensions [grid]
  (let [height (count grid)
        width (if (= height 0) 0 (count (first grid)))]
    (list width height)))

(defn draw-board! [grid]
  ;; base the height and width of the board on the dimensions of grid
  (def height (count grid))
  (def width (if (= height 0) 0 (count (first grid))))

  ;; set the canvas properties accordingly
  (set! (.-height canvas) (+ 1 (* height tile-size)))
  (set! (.-width canvas) (+ 1 (* width tile-size)))

  ;; draw every tile in the grid, tile per tile, row per row
  (mapv
   (fn [y]
     (mapv
      (fn [x]
        (let [tile-state (get-tile grid x y)]
           (draw-tile! (* tile-size x) (* tile-size y) (tile-color tile-state))))
      (range 0 width)))
   (range 0 height)))

;; game of life rules
(defn get-neighbor-count [grid x y]
  (let [dimensions (get-dimensions grid)
        width (first dimensions)
        height (second dimensions)]
    ;; a special get-tile which returns 0 for out of bounds tiles
    (defn tile-value [xy] (let [x (first xy)
                                y (second xy)]
                          (if (or (< x 0) (< y 0) (>= x width) (>= y height))
                            0
                            (nth (nth grid y) x))))
    ;; get the coordinates of all the neighbors, even if they'd fall of the grid
    ;; and then call get-tile on those coordinates
    (def neighbors (mapv (fn [fun] (tile-value (fun x y)))
          [(fn [x y] (list (dec x) (dec y)))
           (fn [x y] (list x (dec y)))
           (fn [x y] (list (inc x) (dec y)))
           (fn [x y] (list (dec x) y))
           ;; (fn [x y] (list x y) <-- this'd be the tile itself, don't count it
           (fn [x y] (list (inc x) y))
           (fn [x y] (list (dec x) (inc y)))
           (fn [x y] (list x (inc y)))
           (fn [x y] (list (inc x) (inc y)))]))
    (reduce + neighbors)))

(defn tile-lives [current-state neighbor-count]
  (cond
    ;; if the cell is dead
    (= current-state 0)
    ;; and it has 3 live neighbors it turns to life
    (if (= neighbor-count 3) 1 0)

    ;; if the cell is alive
    (= current-state 1)
    (cond
      ;; and it has 1 or less live neighbors it dies
      (<= neighbor-count 1) 0
      ;; or if it has 4 or more live neighbors it dies
      (>= neighbor-count 4) 0
      ;; but if it has 2 or 3 live neighbors it stays alive
      :else 1)))

(defn next-generation [grid]
  (let [dimensions (get-dimensions grid)
        width (first dimensions)
        height (second dimensions)]
    (mapv (fn [y] (mapv (fn [x]
                        (let [current-state (get-tile grid x y)
                              neighbor-count (get-neighbor-count grid x y)]
                          (tile-lives current-state neighbor-count)))
                      (range 0 width)))
        (range 0 height))))

(def empty-board (vec (repeat grid-height (vec (repeat grid-width 0)))))
(defn build-board [coords]
  (defn build-board-inter [coords board]
    (if (empty? coords) board
        (build-board-inter (rest coords)
                           (let [coord (first coords)
                                 y (first coord)
                                 x (second coord)
                                 row (nth board y)]
                             (assoc board y (assoc row x 1))))))

  (build-board-inter coords empty-board))
;; thanks to https://github.com/jdomingu/GameOfLifeLisp/blob/master/game-of-life.lsp
(def gosper '((5 1) (5 2) (6 1) (6 2) (5 11) (6 11) (7 11) (4 12) (3 13) (3 14) (8 12) (9 13) (9 14) (6 15) (4 16) (5 17) (6 17) (7 17) (6 18) (8 16) (3 21) (4 21) (5 21) (3 22) (4 22) (5 22) (2 23) (6 23) (1 25) (2 25) (6 25) (7 25) (3 35) (4 35) (3 36) (4 36)))
(def board (build-board gosper))          
;;(def board [
;;           [0 1 0 0]
;;           [1 0 1 1]
;;           [0 0 0 1]])

(defn main-loop [state]
  (draw-board! state)
  (js/setTimeout #(main-loop (next-generation state)) 100))

(main-loop board)


;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
