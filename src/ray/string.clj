(ns ray.string
  (:require
    [clojure.string :as st]))

(defn- shortest-indent [string]
  (as-> string <>
        (st/trim <>)
        (re-seq #"\n\s*" <>)
        (sort-by count <>)
        (nth <> 0)))

(defn $ [string]
  (as-> string a
        (shortest-indent a)
        (st/replace string a "\n")
        (st/trim a)))
