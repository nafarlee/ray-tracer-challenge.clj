(ns ray.matrix)

(defn at [m x y]
  (-> m
      (nth x)
      (nth y)))

(defn row [m r]
  (nth m r))

(defn column [m c]
  (map #(nth % c) m))

(def rows identity)

(defn columns [m]
  (apply mapv vector m))

(defn dot [r c]
  (apply + (mapv * r c)))

(defn multiply [a b]
  (->> (for [r (rows a)
             c (columns b)]
         (dot r c))
       (partition (count (first b)))
       (mapv vec)))

(def transpose columns)

(def id [[1 0 0 0]
         [0 1 0 0]
         [0 0 1 0]
         [0 0 0 1]])

(defn size [m]
  [(count m) (count (first m))])

(defn determinant2 [[[a b]
                     [c d]]]
  (- (* a d) (* b c)))

(defn submatrix [m r c]
  (->> m
       (keep-indexed #(if (== r %1) nil %2))
       (mapv (partial keep-indexed #(if (== c %1) nil %2)))))

(defn minor [m r c]
  (-> m
      (submatrix r c)
      determinant))

(defn cofactor [m r c]
  (as-> m $
        (minor $ r c)
        (if (odd? (+ r c))
          (unchecked-negate $)
          $)))
