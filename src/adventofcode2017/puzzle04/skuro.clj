(ns adventofcode2017.puzzle04.skuro
  (:require [clojure.java.io :refer [reader]]
            [clojure.string :refer [split]]))

(defn only-unique-vals?
  [seq]
  (boolean (reduce (fn [seen? val]
                     (if (seen? val)
                       (reduced false)
                       (conj seen? val)))
                   #{}
                   seq)))

(defn contains-no-anagrams?
  [seq]
  (boolean (reduce (fn [seen? val]
                     (let [letters (frequencies val)]
                       (if (seen? letters)
                         (reduced false)
                         (conj seen? letters))))
                   #{}
                   seq)))

(defn count-valid-passphrases [input]
  (let [line->words (fn [line] (split line #" "))
        lines       (with-in-str input
                      (doall (map line->words (line-seq (reader *in*)))))]
    (count (filter only-unique-vals? lines))))

(defn count-valid-passphrases-2 [input]
  (let [line->words (fn [line] (split line #" "))
        valid?      (fn [words] (and (only-unique-vals? words)
                                     (contains-no-anagrams? words)))
        lines       (with-in-str input
                      (doall (map line->words (line-seq (reader *in*)))))]
    (count (filter valid? lines))))
