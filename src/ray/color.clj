(ns ray.color
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::color (s/tuple (s/tuple float?)
                        (s/tuple float?)
                        (s/tuple float?)))
(defn color [red green blue]
  [[red]
   [green]
   [blue]])
