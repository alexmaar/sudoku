(ns sudoku.core
  (:require [clojure.set :as set])
  (:gen-class))

(def board identity) ;creates board

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))


(def valid-values #{1 2 3 4 5 6 7 8 9})
  
; checking board
(defn value-at
 "returns value at given coordinates"
  [board [row col]]
  (get-in board [row col]))

(defn has-value?
  "returns true if field is not empty"
  [board coordinates]
  (not= 0 (value-at board coordinates)))

(defn row-values
  "returns all used values in given row"
  [board coordinates]
  (set (get board (first coordinates))))

(defn col-values
  "returns all used values in given column"
  [board coordinates]
  (set (map #(get % (last coordinates)) board)))

(defn block-of-coordinates
 "return top left corner of block which contains coordinates" 
  [[row col]]
  [(* (quot row 3) 3) (* 3 (quot col 3))])

(defn coord-pairs [row-sequence col-sequence]
  (for [x row-sequence
        y col-sequence]
    [x y]))

(defn block-values [board [x y]]
  (set (map (partial value-at board)
            (coord-pairs 
             (range (first (block-of-coordinates [x y])) (+ 3 (first (block-of-coordinates [x y]))))
            (range (last (block-of-coordinates [x y])) (+ 3 (last (block-of-coordinates [x y]))))))))

(defn valid-values-for [board [row col]]
  (if (has-value? board [row col])
    #{}
    (set/difference valid-values 
                    (set/union 
                     (block-values board [row col]) 
                     (row-values board [row col]) 
                     (col-values board [row col])))))

(defn filled? [board]
  (empty? (filter #(some #{0} %) board)))

(defn rows [board]
  (map set board))

(defn cols [board]
  (let [cols-cords [[0 0] [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [0 8]]]
    (map (partial col-values board) cols-cords)))

(defn blocks [board]
  (let [blocks-cords [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
    (map (partial block-values board) blocks-cords)))

(defn valid-rows [board]
  (every? #(= % valid-values) (rows board)))

(defn valid-cols [board]
  (every? #(= % valid-values) (cols board)))

(defn valid-blocks [board]
  (every? #(= % valid-values) (cols board)))

(defn valid-solution? [board]
  (and
   (valid-rows board)
   (valid-cols board)
   (valid-blocks board)))

;modyfing board

(defn set-value-at [board [row col] new-value]
  (assoc-in board [row col] new-value))

(defn find-empty-point [board]
  (if (filled? board)
    nil
    [(.indexOf board (first (filter #(some (fn [x] (= x 0)) %) board))) (.indexOf (first (filter #(some (fn [x] (= x 0)) %) board)) 0)]))

(defn solve-board [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (let [empty-field (find-empty-point board)
          valid-values (valid-values-for board empty-field)]
      (for [value valid-values
            solution (solve-board (set-value-at board empty-field value))]
        solution))))

(defn additional-function []
  (println "Working with git branches and command"))

;delete some functions
