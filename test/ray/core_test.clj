(ns ray.core-test
  (:require
   [clojure.string :as st]
   [clojure.test :refer [deftest is testing]]
   [ray.matrix :as matrix]
   [ray.tuple :as rt]
   [ray.color :as rc]
   [ray.canvas :as rcan]
   [ray.ppm :as rp]
   [ray.string :as rs]))

(deftest ray-tracer-challenge-tests

(testing "A tuple with w=1.0 is a point"
  (let [a (rt/tuple 4.3 -4.2 3.1 1.0)]
    (is (== (::rt/x a) 4.3))
    (is (== (::rt/y a) -4.2))
    (is (== (::rt/z a) 3.1))
    (is (== (::rt/w a) 1.0))
    (is (rt/point? a))
    (is (not (rt/vector'? a)))))

(testing "A tuple with w=0.0 is a vector"
  (let [a (rt/tuple 4.3 -4.2 3.1 0.0)]
    (is (== (::rt/x a) 4.3))
    (is (== (::rt/y a) -4.2))
    (is (== (::rt/z a) 3.1))
    (is (== (::rt/w a) 0.0))
    (is (not (rt/point? a))
    (is (rt/vector'? a)))))

(testing "point creates tuples with w=1"
  (let [a (rt/point 4 -4 3)]
    (is (rt/eq a (rt/tuple 4 -4 3 1)))))

(testing "vector creates tuples with w=0"
  (let [a (rt/vector' 4 -4 3)]
    (is (rt/eq a (rt/tuple 4 -4 3 0)))))

(testing "Adding two tuples"
  (let [a1 (rt/tuple 3 -2 5 1)
        a2 (rt/tuple -2 3 1 0)]
    (is (rt/eq (rt/add a1 a2)
                (rt/tuple 1 1 6 1)))))

(testing "Subtracting two points"
  (let [p1 (rt/point 3 2 1)
        p2 (rt/point 5 6 7)]
    (is (rt/eq (rt/subtract p1 p2)
                (rt/vector' -2 -4 -6)))))

(testing "Subtracting a vector from a point"
  (let [p (rt/point 3 2 1)
        v (rt/vector' 5 6 7)]
    (is (rt/eq (rt/subtract p v)
                (rt/point -2 -4 -6)))))

(testing "Subtracting two vectors"
  (let [v1 (rt/vector' 3 2 1)
        v2 (rt/vector' 5 6 7)]
    (is (rt/eq (rt/subtract v1 v2)
                (rt/vector' -2 -4 -6)))))

(testing "Subtracting a vector from the zero vector"
  (let [zero (rt/vector' 0 0 0)
        v (rt/vector' 1 -2 3)]
    (is (rt/eq (rt/subtract zero v)
                (rt/vector' -1 2 -3)))))

(testing "Negating a tuple"
  (let [a (rt/tuple 1 -2 3 -4)]
    (is (rt/eq (rt/negate a)
                (rt/tuple -1 2 -3 4)))))

(testing "Multiplying a tuple by a scalar"
  (let [a (rt/tuple 1 -2 3 -4)]
    (is (rt/eq (rt/multiply a 3.5)
                (rt/tuple 3.5 -7.0 10.5 -14.0)))))

(testing "Multiplying a tuple by a fraction"
  (let [a (rt/tuple 1 -2 3 -4)]
    (is (rt/eq (rt/multiply a 0.5)
                (rt/tuple 0.5 -1.0 1.5 -2.0)))))

(testing "Dividing a tuple by a scalar"
  (let [a (rt/tuple 1.0 -2.0 3.0 -4.0)]
    (is (rt/eq (rt/divide a 2)
                (rt/tuple 0.5 -1.0 1.5 -2.0)))))

(testing "Computing the magnitude of vector (1, 0, 0)"
  (let [v (rt/vector' 1 0 0)]
    (is (== (rt/magnitude v)
            1.0))))

(testing "Computing the magnitude of vector (0, 1, 0)"
  (let [v (rt/vector' 0 1 0)]
    (is (== (rt/magnitude v)
            1.0))))

(testing "Computing the magnitude of vector (0, 0, 1)"
  (let [v (rt/vector' 0 0 1)]
    (is (== (rt/magnitude v)
            1.0))))

(testing "Computing the magnitude of vector (1, 2, 3)"
  (let [v (rt/vector' 1 2 3)]
    (is (== (rt/magnitude v)
            (Math/sqrt 14)))))

(testing "Computing the magnitude of vector (-1, -2, -3)"
  (let [v (rt/vector' -1 -2 -3)]
    (is (== (rt/magnitude v)
            (Math/sqrt 14)))))

(testing "Normalizing (4, 0, 0) gives (1, 0, 0)"
  (let [v (rt/vector' 4 0 0)]
    (is (rt/eq (rt/normalize v)
                (rt/vector' 1.0 0.0 0.0)))))

(testing "Normalizing (1, 2, 3)"
  (let [v (rt/vector' 1 2 3)]
    (is (rt/eq (rt/normalize v)
            (rt/vector' (/ 1 (Math/sqrt 14))
                         (/ 2 (Math/sqrt 14))
                         (/ 3 (Math/sqrt 14)))))))

(testing "The magnitude of a normalized vector"
  (let [v (rt/vector' 1 2 3)
        norm (rt/normalize v)]
    (is (== (rt/magnitude norm)
            1.0))))

(testing "The dot product of two tuples"
  (let [a (rt/vector' 1 2 3)
        b (rt/vector' 2 3 4)]
    (is (== (rt/dot a b)
            20))))

(testing "The cross product of two vectors"
  (let [a (rt/vector' 1 2 3)
        b (rt/vector' 2 3 4)]
    (is (rt/eq (rt/cross a b)
                (rt/vector' -1 2 -1)))
    (is (rt/eq (rt/cross b a)
                (rt/vector' 1 -2 1)))))

(testing "Colors are (red, green, blue) tuples"
  (let [{:keys [::rc/red ::rc/green ::rc/blue]} (rc/color -0.5 0.4 1.7)]
    (is (== red -0.5))
    (is (== green 0.4))
    (is (== blue 1.7))))

(testing "Adding colors"
  (let [c1 (rc/color 0.9 0.6 0.75)
        c2 (rc/color 0.7 0.1 0.25)]
    (is (rt/eq (rt/add c1 c2)
                (rc/color 1.6 0.7 1.0)))))

(testing "Subtracting colors"
  (let [c1 (rc/color 0.9 0.6 0.75)
        c2 (rc/color 0.7 0.1 0.25)]
    (is (rt/eq (rt/subtract c1 c2)
                (rc/color 0.2 0.5 0.5)))))

(testing "Multiplying a color by scalar"
  (let [c (rc/color 0.2 0.3 0.4)]
    (is (rt/eq (rc/color 0.4 0.6 0.8)
                (rt/multiply c 2)))))

(testing "Multiplying colors"
  (let [c1 (rc/color 1 0.2 0.4)
        c2 (rc/color 0.9 1 0.1)]
    (is (rt/eq (rt/hadamard c1 c2)
                (rc/color 0.9 0.2 0.04)))))

(testing "Creating a canvas"
  (let [c (rcan/canvas 10 20)]
    (is (== (::rcan/width c) 10))
    (is (== (::rcan/height c) 20))
    (is (every? (partial rt/eq (rc/color 0 0 0))
                (::rcan/pixels c)))))

(testing "Writing pixels to a canvas"
  (let [c (rcan/canvas 10 20)
        red (rc/color 1 0 0)]
    (is (rt/eq red
                (-> c
                    (rcan/write-pixel 2 3 red)
                    (rcan/pixel-at 2 3))))))

(testing "Constructing the PPM header"
  (let [c (rcan/canvas 5 3)
        ppm (rp/canvas->ppm c)
        header (->> ppm (st/split-lines) (take 3) (st/join "\n"))]
    (is (= header (rs/$ "
                        P3
                        5 3
                        255
                        ")))))

(testing "Constructing the PPM pixel data"
  (let [c (rcan/canvas 5 3)
        c1 (rc/color 1.5 0 0)
        c2 (rc/color 0 0.5 0)
        c3 (rc/color -0.5 0 1)]
    (is (= (as-> c %
               (rcan/write-pixel % 0 0 c1)
               (rcan/write-pixel % 2 1 c2)
               (rcan/write-pixel % 4 2 c3)
               (rp/canvas->ppm %)
               (st/split-lines %)
               (drop 3 %)
               (take 3 %)
               (st/join "\n" %))
           (rs/$ "
                 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 128 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
                 ")))))

(testing "Splitting long lines in PPM files"
  (let [c (rcan/canvas 10 2)
        color (rc/color 1 0.8 0.6)]
    (is (= (as-> c $
                 (reduce (fn [can [x y]] (rcan/write-pixel can x y color))
                         $
                         (for [x (range 10), y (range 2)] [x y]))
                 (rp/canvas->ppm $)
                 (st/split-lines $)
                 (drop 3 $)
                 (take 4 $)
                 (st/join "\n" $))
           (rs/$ "
                 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
                 153 255 204 153 255 204 153 255 204 153 255 204 153
                 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
                 153 255 204 153 255 204 153 255 204 153 255 204 153
                 ")))))

(testing "PPM files are terminated by a newline character"
  (let [c (rcan/canvas 5 3)]
    (is (st/ends-with? (rp/canvas->ppm c)
                       "\n"))))

(testing "Constructing and inspecting a 4x4 matrix"
  (let [M [[1    2    3    4]
           [5.5  6.5  7.5  8.5]
           [9    10   11   12]
           [13.5 14.5 15.5 16.5]]]
    (is (== 1 (matrix/at M 0 0)))
    (is (== 4 (matrix/at M 0 3)))
    (is (== 5.5 (matrix/at M 1 0)))
    (is (== 7.5 (matrix/at M 1 2)))
    (is (== 11 (matrix/at M 2 2)))
    (is (== 13.5 (matrix/at M 3 0)))
    (is (== 15.5 (matrix/at M 3 2)))))

(testing "A 2x2 matrix ought to be representable"
  (let [M [[-3  5]
           [ 1 -2]]]
    (is (== -3 (matrix/at M 0 0)))
    (is (== 5 (matrix/at M 0 1)))
    (is (== 1 (matrix/at M 1 0)))
    (is (== -2 (matrix/at M 1 1)))))

(testing "A 3x3 matrix ought to be representable"
  (let [M [[-3   5  0]
           [ 1  -2 -7]
           [ 0   1  1]]]
    (is (== -3 (matrix/at M 0 0)))
    (is (== -2 (matrix/at M 1 1)))
    (is (== 1 (matrix/at M 2 2)))))

(testing "Matrix equality with identical matrices"
  (let [A [[1 2 3 4]
           [5 6 7 8]
           [9 8 7 6]
           [5 4 3 2]]
        B [[1 2 3 4]
           [5 6 7 8]
           [9 8 7 6]
           [5 4 3 2]]]
    (is (= A B))))

(testing "Matrix equality with different matrices"
  (let [A [[1 2 3 4]
           [5 6 7 8]
           [9 8 7 6]
           [5 4 3 2]]
        B [[2 3 4 5]
           [6 7 8 9]
           [8 7 6 5]
           [4 3 2 1]]]
    (is (not= A B))))

(testing "Multiplying two matrices"
  (let [A [[1 2 3 4]
           [5 6 7 8]
           [9 8 7 6]
           [5 4 3 2]]
        B [[-2 1 2  3]
           [ 3 2 1 -1]
           [ 4 3 6  5]
           [ 1 2 7  8]]
        AB [[20 22 50  48]
            [44 54 114 108]
            [40 58 110 102]
            [16 26 46  42]]]
    (is (= (matrix/multiply A B) AB))))

(testing "A matrix multiplied by a tuple"
  (let [A [[1 2 3 4]
           [2 4 4 2]
           [8 6 4 1]
           [0 0 0 1]]
        b [[1]
           [2]
           [3]
           [1]]
        Ab [[18]
            [24]
            [33]
            [1]]]
    (is (= (matrix/multiply A b) Ab))))

)
