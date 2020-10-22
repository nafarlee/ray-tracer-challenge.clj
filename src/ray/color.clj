(ns ray.color
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::color (s/tuple (s/tuple double?)
                        (s/tuple double?)
                        (s/tuple double?)))
(defn color [red green blue]
  [[red]
   [green]
   [blue]])
