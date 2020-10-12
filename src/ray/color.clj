(ns ray.color
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::red float?)
(s/def ::green float?)
(s/def ::blue float?)
(s/def ::color (s/keys :req [::red ::green ::blue]))
(defn color [red green blue]
  {::red red, ::green green, ::blue blue})
