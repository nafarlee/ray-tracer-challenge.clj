(ns ray.core
  (:require
   [clojure.string :as st]
   [clojure.spec.alpha :as s])
  (:gen-class))

(def EPSILON 0.00001)

(defn float= [a b]
  (->> a
       (- b)
       Math/abs
       (> EPSILON)))

(defn map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(def zip (partial map vector))

(defn eq [a b]
  (every? (partial apply float=)
          (zip (vals a) (vals b))))

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

(s/def ::x float?)
(s/def ::y float?)
(s/def ::z float?)
(s/def ::w #{1.0 0.0})
(s/def ::tuple (s/keys :req [::x ::y ::z ::w]))
(defn tuple [x y z w]
  {::x x, ::y y, ::z z, ::w w})

(def tuple? (partial s/valid? ::tuple))

(defn multiply [t x]
  (map-values (partial * x) t))

(defn divide [t x]
  (map-values #(/ % x) t))

(defn point [x y z]
  (tuple x y z 1))

(defn point?  [{w ::w :as t}]
  (and (tuple? t)
       (== 1.0 w)))

(defn vector' [x y z]
  (tuple x y z 0))

(defn vector'?  [{w ::w :as t}]
  (and (tuple? t)
       (zero? w)))

(s/def ::red float?)
(s/def ::green float?)
(s/def ::blue float?)
(s/def ::color (s/keys :req [::red ::green ::blue]))
(defn color [red green blue]
  {::red red, ::green green, ::blue blue})

(def add (partial merge-with +))

(def subtract (partial merge-with -))

(def negate (partial subtract (vector' 0 0 0)))

(defn square [x]
  (Math/pow x 2))

(defn magnitude [v]
  (->> (vals v)
       (map square)
       (apply +)
       Math/sqrt))

(defn normalize [v]
  (let [m (magnitude v)]
    (->> ((juxt ::x ::y ::z) v)
         (map #(/ % m))
         (apply vector'))))

(defn dot [a b]
  (->> (merge-with * a b)
       vals
       (apply +)))

(defn cross [{ax ::x ay ::y az ::z} {bx ::x by ::y bz ::z}]
  (vector' (- (* ay bz)
              (* az by))
           (- (* az bx)
              (* ax bz))
           (- (* ax by)
              (* ay bx))))

(def hadamard (partial merge-with *))

(s/def ::pixels vector?)
(s/def ::width (s/and pos? integer?))
(s/def ::height (s/and pos? integer?))
(s/def ::canvas (s/keys :req [::pixels ::width ::height]))

(defn ->canvas [p w h]
  {::pixels p, ::width w, ::height h})

(defn canvas [w h]
  (-> (* w h)
      (repeat (color 0 0 0))
      vec
      (->canvas w h)))

(defn write-pixel [{w ::width h ::height ps ::pixels} x y color]
    (-> ps
        (assoc (+ x (* y w)) color)
        (->canvas w h)))

(defn pixel-at [{w ::width ps ::pixels} x y]
  (nth ps (+ x (* y w))))

(defn clamp [low high x]
  (-> x
      (max low)
      (min high)))

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
      $
      (format (::width c) (::height c))))

(defn ppm-body [c]
  (->> c
       ::pixels
       (partition (::width c))
       (map #(->> %
                  (mapcat (juxt ::red ::green ::blue))
                  (map (partial * 255))
                  (map (partial clamp 0 255))
                  (map double)
                  (map (fn [x] (Math/round x)))
                  (str-wrap 70)))
       (st/join "\n")))

(defn canvas->ppm [c]
  (format "%s\n%s\n" (ppm-header c) (ppm-body c)))
