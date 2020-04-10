(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(require '[clojure.core.reducers :as reducers])

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn str-diff [word1 word2]
  (reducers/fold + (for [[c1 c2] (map list word1 word2)] (if (= c1 c2) 0 1))))

(defn lazy-contains? [col key]
  (some #{key} col))

(defn process [word1 word2 words acc]
  (let [pool (get (group-by count words) (count word1))
        next-words (filter #(= (str-diff word1 %) 1) pool)]
    (cond
      (empty? next-words) []
      (lazy-contains? next-words word2) (conj acc word2)
      :else (let [rest-words (filter #(> (str-diff word1 %) 1) pool)]
              (first (sort-by count (filter not-empty (for [w next-words] (process w word2 rest-words (conj acc w))))))))))

(defn doublets [word1 word2]
  (let [pool (get (group-by count words) (count word1))
        next-words (filter #(= (str-diff word1 %) 1) pool)]
    (cond
      (empty? next-words) []
      (lazy-contains? next-words word2) [word1 word2]
      :else (let [rest-words (filter #(> (str-diff word1 %) 1) pool)]
              (first (sort-by count (for [w next-words] (process w word2 rest-words [word1 w]))))))))
