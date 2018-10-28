(ns ^:figwheel-hooks xyz.vanduuren.game-of-clojure
  (:require
   [goog.dom :as gdom]))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(def canvas (gdom/getElement "canvas"))
(def ctx (.getContext canvas "2d"))
(def tile-size 10)
(def color-on "darkgrey")
(def color-off "white")
(def grid-height 22)
(def grid-width 42)

(defn tile-color
  "Determine the color of a cell, return color-off if the value if 0, color-on if it's 1"
  [bit] (if (= bit 1) color-on color-off))

(defn draw-tile!
  "Draw a tile on our canvas, its dimensions are taken from the context. Draw it at x,y with color."
  [x y color]
  
  ;; workaround to get crisp grid lines
  ;; (we won't see it for now, but lets keep it in for reference)
  (.setTransform ctx 1, 0, 0, 1, 0.5, 0.5)
  
  ;; begin drawing a new path in the canvas
  (.beginPath ctx)
  (.rect ctx x y tile-size tile-size)

  (set! (.-fillStyle ctx) color)
  (.fill ctx)

  (set! (.-lineWidth ctx) 0.5)
  (set! (.-strokeStyle ctx) color-off)
  (.stroke ctx))

(defn tile-value "Get the value of cell with coords x, y in the given grid" [x y grid] (nth (nth grid y) x))
(defn get-dimensions "Get the dimensions of the given grid" [grid]
  (let [height (count grid)
        width (if (= height 0) 0 (count (first grid)))]
    (list width height)))

(defn draw-board! "Given an initial grid, start the Game of Life!"
  [grid]
  
  ;; base the height and width of the board on the dimensions of grid
  (def height (count grid))
  (def width (if (= height 0) 0 (count (first grid))))
  ;; set an invisible border of 2
  (def invisible-border 2)

  ;; set the canvas properties accordingly
  (set! (.-height canvas) (+ 1 (* height tile-size)))
  (set! (.-width canvas) (+ 1 (* width tile-size)))

  (defn in-invisible-border [x y thickness]
    "We keep an invisible border of 'thickness' cells on the grid, this is so cells leave the grid gracefully"
    (or (< x thickness) (< y thickness) (>= x (- grid-width thickness)) (>= y (- grid-height thickness))))
  
  ;; draw every tile in the grid, tile per tile, row per row
  (mapv
   (fn [y]
     (mapv
      (fn [x]
        (let [tile-state (tile-value  x y grid)]
          (draw-tile! (* tile-size x) (* tile-size y)
                      (if (in-invisible-border x y invisible-border) (tile-color 0) (tile-color tile-state)))))
      (range 0 width)))
   (range 0 height)))

;; game of life rules
(defn get-neighbor-count "Get the number of live neighbord of cell with coords x,y in grid"
  [grid x y]
  
  (let [dimensions (get-dimensions grid)
        width (first dimensions)
        height (second dimensions)]
    ;; a special tile-value which returns 0 for out of bounds tiles
    (defn safe-tile-value [xy] (let [x (first xy)
                                     y (second xy)]
                                 (if (or (< x 0) (< y 0) (>= x width) (>= y height))
                                   0
                                   (tile-value x y grid))))
    ;; get the coordinates of all the neighbors, even if they'd fall of the grid
    ;; and then call tile-value on those coordinates
    ;; we do this by having a collection of functions that shift the coordinates of
    ;; the current cell to the coordinates of its neighbors, and then we call those
    ;; functions in our mapping function to get the actual coordinates. Those are
    ;; then used as input for safe-tile-value
    ;; TODO: probably easier with matrix arithmetic
    (def neighbors (mapv (fn [fun] (safe-tile-value (fun x y)))
                         [(fn [x y] (list (dec x) (dec y)))
                          (fn [x y] (list x (dec y)))
                          (fn [x y] (list (inc x) (dec y)))
                          (fn [x y] (list (dec x) y))
                          ;; (fn [x y] (list x y) <-- this'd be the tile itself, don't count it
                          (fn [x y] (list (inc x) y))
                          (fn [x y] (list (dec x) (inc y)))
                          (fn [x y] (list x (inc y)))
                          (fn [x y] (list (inc x) (inc y)))]))
    ;; above leaves us with the number of live neighoring cells, which we then
    ;; all add up using reduce with the addition (+) function
    (reduce + neighbors)))

(defn tile-lives "Given a current state of a cell and the amount of neighbors decide if it lives, dies,
  or spring to life"
  [current-state neighbor-count]
  
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

(defn next-generation "Calculate the next generation of the grid.
  This is done sequentially and can be optimized. Might be a nice exercise for the future."
  [grid]
  
  (let [dimensions (get-dimensions grid)
        width (first dimensions)
        height (second dimensions)]
    (mapv (fn [y] (mapv (fn [x]
                          (let [current-state (tile-value x y grid)
                                neighbor-count (get-neighbor-count grid x y)]
                            (tile-lives current-state neighbor-count)))
                        (range 0 width)))
          (range 0 height))))

(def empty-board "Helper function to create an empty board"
  (vec (repeat grid-height (vec (repeat grid-width 0)))))

(defn build-board "Helper function to create a board with live cells based on a list of tuples"
  [coords]
  (defn build-board-inter [coords board]
    (if (empty? coords) board
        (build-board-inter (rest coords)
                           (let [coord (first coords)
                                 y (first coord)
                                 x (second coord)
                                 row (nth board y)]
                             (assoc board y (assoc row x 1))))))

  (build-board-inter coords empty-board))

(def gosper "Coordinates for Gosper's gliding gun, thanks
  to https://github.com/jdomingu/GameOfLifeLisp/blob/master/game-of-life.lsp"
  '((5 2) (5 3) (6 2) (6 3) (5 12) (6 12) (7 12) (4 13) (3 14) (3 15) (8 13) (9 14) (9 15) (6 16) (4 17) (5 18) (6 18) (7 18) (6 19) (8 17) (3 22) (4 22) (5 22) (3 23) (4 23) (5 23) (2 24) (6 24) (1 26) (2 26) (6 26) (7 26) (3 36) (4 36) (3 37) (4 37)))

(def board "Build a board using gosper's coordinates" (build-board gosper))          

(defn main-loop "The main loop, calls itself every 100ms with the next grid"
  [state]
  
  (draw-board! state)
  (js/setTimeout #(main-loop (next-generation state)) 100))

;; Start the main loop
(main-loop board)


;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
