(ns adventofcode2017.puzzle02.skuro
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn lines [input]
  (with-in-str input
    (doall (line-seq (io/reader *in*)))))

(defn line->nums [line]
  (map #(Long/parseLong %)
       (str/split line #"\t")))

(defn watermarks [{:keys [low high]} num]
  {:low  ((fnil min num) low num)
   :high ((fnil max 0) high num)})

(defn line-checksum [nums]
  (let [{:keys [low high]} (reduce watermarks nums)]
    (- high low)))

(defn checksum [input]
  (reduce + 0
          (->> (lines input)
               (map line->nums)
               (map line-checksum))))

(defn even-division [nums]
  (for [x nums
        y nums
        :when (and (not= x y)
                   (= 0 (mod x y)))]
    (/ x y)))

(defn solution-2 [input]
  (reduce + 0
          (->> input
               lines
               (map line->nums)
               (mapcat even-division))))
