(ns ray.ppm
  (:require
    [clojure.string :as st]
    [clojure.test :refer [is]]
    [ray.string :as rs]
    [ray.math :refer [clamp round]]))

(defn str-wrap [max-length xs]
  (->> (reduce (fn [[s & lines] a]
                 (let [potential (str s " " a)]
                   (if (> (count potential) max-length)
                     (cons (str a) (cons s lines))
                     (cons potential lines))))
               (take 1 xs)
               (rest xs))
       reverse
       (st/join "\n")))

(defn ppm-header [c]
  (-> "
      P3
      %s %s
      255
      "
      rs/$
      (format (:width c) (:height c))))

(defn- colors->lace [cs]
  (->> cs
       flatten
       (map (partial * 255))
       (map (partial clamp 0 255))
       (map double)
       (map round)
       (str-wrap 70)))

(defn ppm-body [{:keys [pixels width]}]
  {:pre [(is (vector? pixels))
         (is (number? width))]}
  (->> pixels
       (partition width)
       (map colors->lace)
       (st/join "\n")))

(defn canvas->ppm [c]
  (format "%s\n%s\n" (ppm-header c) (ppm-body c)))
