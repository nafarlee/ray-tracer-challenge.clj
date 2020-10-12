(ns ray.string
  (:require
    [clojure.string :as st]))

(defn- shortest-indent [string]
  (->> string
       st/trim
       (re-seq #"\n\s*")
       (sort-by count)
       first))

(defn $ [string]
  (as-> string a
        (shortest-indent a)
        (st/replace string a "\n")
        (st/trim a)))
