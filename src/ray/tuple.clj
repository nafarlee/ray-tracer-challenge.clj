(ns ray.tuple
  (:require
    [clojure.spec.alpha :as s]
    [ray.matrix :as m]
    [ray.math :refer [float= square]]))

(defn tuple [& ds]
  (mapv (comp vector double) ds))

(def tuple?
  (partial s/valid?
           (s/coll-of (s/tuple double?) :kind vector?)))

(defn magnitude [v]
  (->> v
       flatten
       (mapv square)
       (apply +)
       Math/sqrt))

(defn normalize [v]
  (let [m (magnitude v)]
     (m/fmap (fn [e _ _] (/ e m)) v)))

(defn dot [a b]
  (->> (m/hadamard a b)
       flatten
       (apply +)))
