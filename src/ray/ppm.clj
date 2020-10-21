(ns ray.ppm
  (:require
    [clojure.string :as st]
    [ray.string :as rs]
    [ray.math :refer [clamp]]
    [ray.canvas :as rcan]
    [ray.color :as rc]
    ))

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
      (format (::rcan/width c) (::rcan/height c))))

(defn ppm-body [c]
  (->> c
       ::rcan/pixels
       (partition (::rcan/width c))
       (map #(->> %
                  flatten
                  (map (partial * 255))
                  (map (partial clamp 0 255))
                  (map double)
                  (map (fn [x] (Math/round x)))
                  (str-wrap 70)))
       (st/join "\n")))

(defn canvas->ppm [c]
  (format "%s\n%s\n" (ppm-header c) (ppm-body c)))
