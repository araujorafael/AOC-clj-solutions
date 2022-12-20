(ns day-4.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn coerce-line [line-str]
  (->> line-str
       (re-find #"(\d+)-(\d+),(\d+)-(\d+)")
       (drop 1)
       (map #(. Integer parseInt %))
       (split-at 2)))

(defn load-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (doall (map coerce-line (line-seq rdr)))))

(defn has-full-overlap? [ids]
  (let [[first-sector last-sector] (first ids)
        [first-sector* last-sector*] (last ids)]
    (or (and (>= first-sector first-sector*) (<= last-sector last-sector*))
        (and (>= first-sector* first-sector) (<= last-sector* last-sector)))))

(defn get-full-overlaps [input]
  (filter has-full-overlap? input))

(defn range'
  "Use `start` and `end` as predicates (both integers) and fill the missing numbers between then.
   I could use the `clojure.core/range` fn but i think it will be
   interesting to create my own implementation"
  [start end]
  ;; Its a non tail safe implementation
  ;; (cons start
  ;;       (if (= start end)
  ;;         '()
  ;;         (range' (inc start) end))))

  ;; Tail safe recursion using clojure recur hack
  (loop [agg '()
         value end]
    (let [new-agg (conj agg value)]
      (if (= value start)
        new-agg
        (recur new-agg
               (dec value))))))

(defn has-overlap? [[pair1 pair2]]
  (let [sequence1 (set (apply range' pair1))
        sequence2 (set (apply range' pair2))]
    (seq (set/intersection sequence1 sequence2))))

(defn find-overlap-signements [input]
  (filter has-overlap? input))


(def ids
  (mapv
   coerce-line
   ["2-4,6-8"
    "2-3,4-5"
    "5-7,7-9"
    "2-8,3-7"
    "6-6,4-6"
    "2-6,4-8"]))

(defn -main [file-path]
  (let [input (load-file file-path)]
    (println "First Puzzle:" (count (get-full-overlaps input)))
    (println "Second Puzzle:" (count (find-overlap-signements input)))))
