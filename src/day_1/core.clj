(ns day-1.core
  (:require [clojure.java.io :as io]))

(defn read-file [path]
  (with-open [rdr (io/reader path)]
    (doall (line-seq rdr))))

(defn calculate-total-cal-per-elf [lines]
  (loop [[cal & cal-list] lines
         carring-calories-count 0
         elfs-calories []]
    (if-not (seq cal-list)
      elfs-calories
      (if-not (seq cal)
        (recur cal-list 0 (conj elfs-calories carring-calories-count))
        (recur cal-list
               (+ carring-calories-count (read-string cal))
               elfs-calories)))))

(defn first-puzzle [calories-per-elf]
  (reduce
      (fn [greater value]
        (if (< greater value)
          value
          greater))
      calories-per-elf))


(defn second-puzzle [calories-per-elf]
  (->> calories-per-elf
       sort
       (take-last 3)
       (reduce +)))

(defn -main [file-path]
  (let [raw-data (read-file file-path)
        calories-per-elf (calculate-total-cal-per-elf raw-data)]
    (println "First Puzzle: " (first-puzzle calories-per-elf))
    (println "Second Puzzle: " (second-puzzle calories-per-elf))))
