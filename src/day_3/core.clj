(ns day-3.core
  (:require [clojure.java.io :as io]
            [clojure.set :as str]))

(defn coerce-line [line]
  (let [line-size (/ (count line) 2)]
    (conj []
          (set (take line-size line))
          (set (drop line-size line)))))

(defn read-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (doall (map coerce-line (line-seq rdr)))))

(defn get-priority-number [char-item]
  (let [item (int char-item)]
    (cond
      (and (<= (int \a) item) (>= (int \z) item)) (- item 96)
      (and (<= (int \A) item) (>= (int \Z) item)) (- item 38))))

(defn find-duplicate-itens [input]
  (reduce
   (fn [agg [first-rack second-rack]]
     (concat
      agg
      (clojure.set/intersection first-rack second-rack)))
   #{}
   input))

(defn sum-priorities [duplicates]
  (reduce
   (fn [agg value]
     (+ agg (get-priority-number value)))
   0
   duplicates))

;; ------ second puzzle
(defn find-group-badge [carry-list]
  (apply clojure.set/intersection (vec carry-list)))

(defn read-file' [file-path]
  (with-open [rdr (io/reader file-path)]
    (doall (map set (line-seq rdr)))))

(defn list-badges [input]
  (loop [agg '()
         carry-list input]
    (if-not (empty? carry-list)
      (recur (concat agg (find-group-badge (take 3 carry-list)))
             (drop 3 carry-list))
      agg)))


(defn first-puzzle [inputs]
  (sum-priorities (find-duplicate-itens inputs)))

(defn second-puzzle [input]
  (-> (list-badges input)
      sum-priorities))

(defn -main [path-file]
  (println "First Puzzle" (first-puzzle (read-file path-file)))
  (println "Second Puzzle" (second-puzzle (read-file' path-file))))
