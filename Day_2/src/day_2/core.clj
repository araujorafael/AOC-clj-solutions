(ns day-2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Load input file functions
(defn corece-input-line [line]
  (let [[opponent me] (str/split line #"\s")]
    (conj [] (keyword opponent) (keyword me))))

(defn load-file [input-path-file]
  (with-open [rdr (io/reader input-path-file)]
    (doall
     (map #(corece-input-line %)
          (line-seq rdr)))))

;; rock can be represented as: A X
;; paper can be represented as: B Y
;; scissors can be represented as: C Z
(def shapes
  {:X [:C :A]
   :Y [:A :B]
   :Z [:B :C]})

(def shape-points
  {:X 1
   :Y 2
   :Z 3})

(defn judge-match-result [[opponent my-shape]]
  (let [[win draw] (get shapes my-shape)]
    (cond
      (= win opponent) 6
      (= draw opponent) 3
      :else 0)))

(defn calc-match-points [[_ my-shape :as line]]
  (+ (judge-match-result line)
     (get shape-points my-shape)))


(defn first-challange [input]
 (reduce
   (fn [agg line]
     (+ agg (calc-match-points line)))
   0
   input))

;; ------
(def match-rigged-points
  {:X 0
   :Y 3
   :Z 6})

(def shape-conditions
  {:A [:C :A :B]
   :B [:A :B :C]
   :C [:B :C :A]})

(def rigged-shape-points
 {:A 1
  :B 2
  :C 3})

(defn calculate-rigged-match [[opponent match-result]]
  (let [match-points (get match-rigged-points match-result)
        [loss draw win] (get shape-conditions opponent)]
    (cond
      (= :X match-result) (+ match-points (get rigged-shape-points loss))
      (= :Y match-result) (+ match-points (get rigged-shape-points draw))
      (= :Z match-result) (+ match-points (get rigged-shape-points win)))))

(defn second-challange [input]
  (reduce
      (fn [agg line]
        (+ agg (calculate-rigged-match line)))
      0
      input))

(defn -main [input-path-file]
  (let [input (load-file input-path-file)]
    (println "First Answer: " (first-challange input))
    (println "Second Answer: " (second-challange input))))
