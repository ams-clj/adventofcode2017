(ns adventofcode2017.puzzle03.skuro
  (:use [clojure.test]))

(with-test
  (defn grid-coordinates
    "Math taken from https://math.stackexchange.com/a/163101/479505"
    [num]
    (let [block-num    (Math/ceil (/ (dec (Math/sqrt num)) 2))
          block-side   (inc (* 2 block-num))
          max-in-block (Math/pow block-side 2)]
      (map int
           (cond
             ;; lower side
             (<= (- max-in-block
                    (dec block-side))
                 num)
             [(- block-num (- max-in-block num))
              (* -1 block-num)]

             ;; left side
             (<= (- max-in-block
                    (* 2 (dec block-side)))
                 num)
             [(* -1 block-num)
              (+ (* -1 block-num)
                 (- max-in-block (dec block-side) num))]

             ;; top side
             (<= (- max-in-block
                    (* 3 (dec block-side)))
                 num)
             [(+ (* -1 block-num)
                 (- max-in-block (* 2 (dec block-side)) num))
              block-num]

             ;; right side
             :else
             [block-num
              (- block-num
                 (- max-in-block num (* 3 (dec block-side))))]))))
  (testing "Grid generation for the first full circle"
    (are [num coords] (= coords (grid-coordinates num))
      1 [0   0]
      2 [1   0]
      3 [1   1]
      4 [0   1]
      5 [-1  1]
      6 [-1  0]
      7 [-1 -1]
      8 [0  -1]
      9 [1  -1])))

(defn grid-distance [num]
  (let [[x y] (grid-coordinates num)]
    (+ (Math/abs x)
       (Math/abs y))))

(defn grid-at [grid [x y]]
  (or (get-in grid [x y])
      0))

(defn gen-num [grid step]
  (let [[x y] (grid-coordinates step)]
    (+ (grid-at grid [x       (dec y)])
       (grid-at grid [(inc x) (dec y)])
       (grid-at grid [(inc x)       y])
       (grid-at grid [(inc x) (inc y)])
       (grid-at grid [x       (inc y)])
       (grid-at grid [(dec x) (inc y)])
       (grid-at grid [(dec x)       y])
       (grid-at grid [(dec x) (dec y)]))))

(with-test
  (defn gen-grid [up-to]
    (loop [grid {0 {0 1}}
           step 2]
      (let [val (gen-num grid step)]
        (if (< up-to val)
          val
          (recur (assoc-in grid (grid-coordinates step) val)
                 (inc step))))))
  (testing "first few grid iterations"
    (are [input result] (= result (gen-grid input))

      0 {0 {0 1}}

      1 {0 {0 1}
         1 {0 1}}

      2 {0 {0 1}
         1 {0 1
            1 2}}

      3 {0 {0 1}
         1 {0 1
            1 2}}

      4 {0 {0 1
            1 4}
         1 {0 1
            1 2}})))

(defn draw-grid [grid]
  (for [line (map second (sort-by first grid))]
    (println (apply str (interleave (map second (sort-by first line))
                                    (repeat "\t"))))))
