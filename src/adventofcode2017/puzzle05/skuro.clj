(ns adventofcode2017.puzzle05.skuro
  (:require [clojure.test :refer [with-test testing is run-tests]]
            [clojure.string :refer [split-lines split join]]))

(defn escaped? [{:keys [maze pos]}]
  (= :escaped
     (nth maze pos :escaped)))

(defn move [{:keys [maze pos counter] :as current}]
  (if (escaped? current)
    (reduced current)
    (let [op       (nth maze pos)
          new-step {:maze    (assoc maze pos (inc op))
                    :pos     (+ pos op)
                    :counter (inc counter)}]
      new-step)))

(defn move-2 [{:keys [maze pos counter] :as current}]
  (if (escaped? current)
    (reduced current)
    (let [op       (nth maze pos)
          new-step {:maze    (assoc maze pos (cond
                                               (<= 3 op) (dec op)
                                               :else     (inc op)))
                    :pos     (+ pos op)
                    :counter (inc counter)}]
      new-step)))

(defn input->maze [input]
  (into [] (map read-string (split-lines input))))

(with-test
  (defn escape-maze [input]
    (loop [maze {:maze    (input->maze input)
                 :pos     0
                 :counter 0}]
      (let [moved (move maze)]
        (if (reduced? moved)
          @moved
          (recur moved)))))
  (testing "Sample maze"
    (is (= {:maze   [2 5 0 1 -2]
            :pos     5
            :counter 5}
           (escape-maze (join \newline [0 3 0 1 -3]))))))

(with-test
  (defn escape-maze-2 [input]
    (loop [maze {:maze    (input->maze input)
                 :pos     0
                 :counter 0}]
      (let [moved (move-2 maze)]
        (if (reduced? moved)
          @moved
          (recur moved)))))
  (testing "Sample maze"
    (is (= {:maze   [2 3 2 3 -1]
            :pos     5
            :counter 10}
           (escape-maze-2 (join \newline [0 3 0 1 -3]))))))
