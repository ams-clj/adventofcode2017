(ns adventofcode2017.puzzle01.skuro
  "Fairly inefficient solution, does multiple processing steps. Still does the job.")

(defn crunch-numbers [input drop-size]
  (let [rest-input (drop drop-size (cycle input))]
    (reduce + 0
            (->> (map (fn when-equals [num nnum]
                        (when (= num nnum)
                          num))
                      input rest-input)
                 (filter identity)
                 (map #(Character/getNumericValue %))))))


(defn solution [input]
  (crunch-numbers input 1))

(defn solution-2 [input]
  (crunch-numbers input (/ (count input) 2)))
