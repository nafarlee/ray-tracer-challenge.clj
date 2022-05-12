(ns ray.core-test
  (:require
   pjstadig.humane-test-output
   [clojure.test :refer [deftest is testing]]
   [ray.transform :refer [view-transform]]
   [ray.world :refer [world color-at shade-hit default-world intersect-world]]
   [ray.material :refer [material]]
   [ray.light :refer [->PointLight lighting]]
   [ray.shape :refer [hit
                      intersect
                      intersection
                      intersections
                      normal-at
                      prepare-computations
                      sphere]]
   [ray.ray :refer [direction origin position ray transform]]
   [ray.math :refer [pi sqrt]]
   [ray.matrix :as matrix]
   [ray.tuple :as tuple]
   [ray.color :as rc]
   [ray.vector3 :refer [vector3]]
   [ray.point3 :refer [point3]]))

(pjstadig.humane-test-output/activate!)

(deftest chapter-three
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
          b (point3 1.0 2.0 3.0)
          Ab (point3 18.0 24.0 33.0)]
      (is (= (matrix/multiply A b) Ab))))

  (testing "Multiplying a matrix by the identity matrix"
    (let [A [[0 1 2  4]
             [1 2 4  8]
             [2 4 8  16]
             [4 8 16 32]]]
      (is (= (matrix/multiply A matrix/id) A))))

  (testing "Multiplying the identity matrix by a tuple"
    (let [a (tuple/tuple 1 2 3 4)]
      (is (= (matrix/multiply matrix/id a) a))))

  (testing "Transposing a matrix"
    (let [A [[0 9 3 0]
             [9 8 0 8]
             [1 8 5 3]
             [0 0 5 8]]]
      (is (= (matrix/transpose A) [[0 9 1 0]
                                   [9 8 8 0]
                                   [3 0 5 5]
                                   [0 8 3 8]]))))

  (testing "Transposing the identity matrix"
    (let [A (matrix/transpose matrix/id)]
      (is (= A matrix/id))))

  (testing "Calculating the determinant of a 2x2 matrix"
    (let [A [[1 5]
             [-3 2]]]
      (is (= (matrix/determinant A) 17))))

  (testing "A submatrix of a 3x3 matrix is a 2x2 matrix"
    (let [A [[ 1 5  8]
             [-3 2  7]
             [ 0 6 -3]]]
      (is (= (matrix/submatrix A 0 2) [[-3 2]
                                       [ 0 6]]))))

  (testing "A submatrix of a 4x4 matrix is a 3x3 matrix"
    (let [A [[-6 1 1 6]
             [-8 5 8 6]
             [-1 0 8 2]
             [-7 1 -1 1]]]
      (is (= (matrix/submatrix A 2 1) [[-6  1 6]
                                       [-8  8 6]
                                       [-7 -1 1]]))))

  (testing "Calculating a minor of a 3x3 matrix"
    (let [A [[3  5  0]
             [2 -1 -7]
             [6 -1  5]]
          B (matrix/submatrix A 1 0)]
      (is (= 25 (matrix/minor A 1 0) (matrix/determinant B)))))

  (testing "Calculating a cofactor of a 3x3 matrix"
    (let [A [[3  5  0]
             [2 -1 -7]
             [6 -1  5]]]
      (is (= -12 (matrix/minor A 0 0)))
      (is (= -12 (matrix/cofactor A 0 0)))
      (is (= 25 (matrix/minor A 1 0)))
      (is (= -25 (matrix/cofactor A 1 0)))))

  (testing "Calculating the determinant of a 3x3 matrix"
    (let [A [[ 1 2  6]
             [-5 8 -4]
             [ 2 6  4]]]
      (is (= 56 (matrix/cofactor A 0 0)))
      (is (= 12 (matrix/cofactor A 0 1)))
      (is (= -46 (matrix/cofactor A 0 2)))
      (is (= -196 (matrix/determinant A)))))

  (testing "Calculating the determinant of a 4x4 matrix"
    (let [A [[-2 -8  3  5]
             [-3  1  7  3]
             [ 1  2 -9  6]
             [-6  7  7 -9]]]
      (is (= 690 (matrix/cofactor A 0 0)))
      (is (= 447 (matrix/cofactor A 0 1)))
      (is (= 210 (matrix/cofactor A 0 2)))
      (is (= 51 (matrix/cofactor A 0 3)))
      (is (= -4071 (matrix/determinant A)))))

  (testing "Testing an invertible matrix for invertibility"
    (let [A [[6  4 4  4]
             [5  5 7  6]
             [4 -9 3 -7]
             [9  1 7 -6]]]
      (is (= -2120 (matrix/determinant A)))
      (is (matrix/invertible? A))))

  (testing "Testing a noninvertible matrix for invertibility"
    (let [A [[-4  2 -2 -3]
             [ 9  6  2  6]
             [ 0 -5  1 -5]
             [ 0  0  0  0]]]
      (is (zero? (matrix/determinant A)))
      (is (not (matrix/invertible? A)))))

  (testing "Calculating the inverse of a matrix"
    (let [A [[-5 2 6 -8]
             [1 -5 1 8]
             [7 7 -6 -7]
             [1 -3 7 4]]
          B (matrix/inverse A)]
      (is (= 532 (matrix/determinant A)))
      (is (= -160 (matrix/cofactor A 2 3)))
      (is (= -160/532 (matrix/at B 3 2)))
      (is (= 105 (matrix/cofactor A 3 2)))
      (is (= 105/532 (matrix/at B 2 3)))
      (is (= [[ 0.21805  0.45113  0.24060 -0.04511]
              [-0.80827 -1.45677 -0.44361  0.52068]
              [-0.07895 -0.22368 -0.05263  0.19737]
              [-0.52256 -0.81391 -0.30075  0.30639]]
             (matrix/fmap #(->> % double (format "%.5f") Double.)
                          B)))))

  (testing "Calculating the inverse of another matrix"
    (let [A [[8 -5 9 2]
             [7 5 6 1]
             [-6 0 9 6]
             [-3 0 -9 -4]]]
      (is (= [[-0.15385 -0.15385 -0.28205 -0.53846]
              [-0.07692  0.12308  0.02564  0.03077]
              [ 0.35897  0.35897  0.43590  0.92308]
              [-0.69231 -0.69231 -0.76923 -1.92308]]
             (matrix/fmap #(->> % double (format "%.5f") Double.)
                          (matrix/inverse A))))))

  (testing "Calculating the inverse of a third matrix"
    (let [A [[ 9  3  0  9]
             [-5 -2 -6 -3]
             [-4  9  6  4]
             [-7  6  6  2]]]
      (is (= [[-0.04074 -0.07778  0.14444 -0.22222]
              [-0.07778  0.03333  0.36667 -0.33333]
              [-0.02901 -0.14630 -0.10926  0.12963]
              [ 0.17778  0.06667 -0.26667  0.33333]]
             (matrix/fmap #(->> % double (format "%.5f") Double.)
                          (matrix/inverse A))))))

  (testing "Multiplying a product by its inverse"
    (let [A [[ 3 -9  7  3]
             [ 3 -8  2 -9]
             [-4  4  4  1]
             [-6  5 -1  1]]
          B [[8  2 2 2]
             [3 -1 7 0]
             [7  0 5 4]
             [6 -2 0 5]]
          C (matrix/multiply A B)]
      (is (= (matrix/multiply C (matrix/inverse B))
             A)))))
  

(deftest chapter-four
  (testing "Multiplying by a translation matrix"
    (let [transform (matrix/translation 5 -3 2)
          p (point3 -3 4 5)]
      (is (= (matrix/multiply transform p)
             (point3 2 1 7)))))

  (testing "Multiplying by the inverse of a translation matrix"
    (let [transform (matrix/translation 5 -3 2)
          inv (matrix/inverse transform)
          p (point3 -3 4 5)]
      (is (= (matrix/multiply inv p)
             (point3 -8 7 3)))))

  (testing "Translation does not affect vectors"
    (let [transform (matrix/translation 5 -3 2)
          v (vector3 -3 4 5)]
      (is (= (matrix/multiply transform v)
             v))))

  (testing "A scaling matrix applied to a point"
    (let [transform (matrix/scaling 2 3 4)
          p (point3 -4 6 8)]
      (is (= (matrix/multiply transform p)
             (point3 -8 18 32)))))

  (testing "A scaling matrix applied to a vector"
    (let [transform (matrix/scaling 2 3 4)
          v (vector3 -4 6 8)]
      (is (= (matrix/multiply transform v)
             (vector3 -8 18 32)))))

  (testing "Multiplying by the inverse of a scaling matrix"
    (let [transform (matrix/scaling 2 3 4)
          inv (matrix/inverse transform)
          v (vector3 -4 6 8)]
      (is (= (matrix/multiply inv v)
             (vector3 -2 2 2)))))

  (testing "Reflection is scaling by a negative value"
    (let [transform (matrix/scaling -1 1 1)
          p (point3 2 3 4)]
      (is (= (matrix/multiply transform p)
             (point3 -2 3 4)))))

  (testing "Rotating a point around the x axis"
    (let [p (point3 0 1 0)
          half-quarter (matrix/rotation-x (/ pi 4))
          full-quarter (matrix/rotation-x (/ pi 2))]
      (is (matrix/eq (matrix/multiply half-quarter p)
                     (point3 0
                             (/ (sqrt 2) 2)
                             (/ (sqrt 2) 2))))
      (is (matrix/eq (matrix/multiply full-quarter p)
                     (point3 0 0 1)))))

  (testing "The inverse of an x-rotation rotates in the opposite direction"
    (let [p (point3 0 1 0)
          half-quarter (matrix/rotation-x (/ pi 4))
          inv (matrix/inverse half-quarter)]
      (is (matrix/eq (matrix/multiply inv p)
                     (point3 0
                                  (/ (sqrt 2) 2)
                                  (- (/ (sqrt 2) 2)))))))

  (testing "Rotating a point around the y axis"
    (let [p (point3 0 0 1)
          half-quarter (matrix/rotation-y (/ pi 4))
          full-quarter (matrix/rotation-y (/ pi 2))]
      (is (matrix/eq (matrix/multiply half-quarter p)
                     (point3 (/ (sqrt 2) 2)
                             0
                             (/ (sqrt 2) 2))))
      (is (matrix/eq (matrix/multiply full-quarter p)
                     (point3 1 0 0)))))

  (testing "Rotating a point around the z axis"
    (let [p (point3 0 1 0)
          half-quarter (matrix/rotation-z (/ pi 4))
          full-quarter (matrix/rotation-z (/ pi 2))]
      (is (matrix/eq (matrix/multiply half-quarter p)
                     (point3 (- (/ (sqrt 2) 2))
                             (/ (sqrt 2) 2)
                             0)))
      (is (matrix/eq (matrix/multiply full-quarter p)
                     (point3 -1 0 0)))))

  (testing "A shearing transformation moves x in proportion to y"
    (let [transform (matrix/shearing 1 0 0 0 0 0)
          p (point3 2 3 4)]
      (is (matrix/eq (matrix/multiply transform p)
                     (point3 5 3 4)))))

  (testing "A shearing transformation moves x in proportion to z"
    (let [transform (matrix/shearing 0 1 0 0 0 0)
          p (point3 2 3 4)]
      (is (matrix/eq (matrix/multiply transform p)
                     (point3 6 3 4)))))

  (testing "A shearing transformation moves y in proportion to x"
    (let [transform (matrix/shearing 0 0 1 0 0 0)
          p (point3 2 3 4)]
      (is (matrix/eq (matrix/multiply transform p)
                     (point3 2 5 4)))))

  (testing "A shearing transformation moves y in proportion to z"
    (let [transform (matrix/shearing 0 0 0 1 0 0)
          p (point3 2 3 4)]
      (is (matrix/eq (matrix/multiply transform p)
                     (point3 2 7 4)))))

  (testing "A shearing transformation moves z in proportion to x"
    (let [transform (matrix/shearing 0 0 0 0 1 0)
          p (point3 2 3 4)]
      (is (matrix/eq (matrix/multiply transform p)
                     (point3 2 3 6)))))

  (testing "A shearing transformation moves z in proportion to y"
    (let [transform (matrix/shearing 0 0 0 0 0 1)
          p (point3 2 3 4)]
      (is (matrix/eq (matrix/multiply transform p)
                     (point3 2 3 7)))))

  (testing "Individual transformations are applied in sequence"
    (let [p (point3 1 0 1)
          A (matrix/rotation-x (/ pi 2))
          B (matrix/scaling 5 5 5)
          C (matrix/translation 10 5 7)
          p2 (matrix/multiply A p)
          p3 (matrix/multiply B p2)
          p4 (matrix/multiply C p3)]
      (is (matrix/eq p2 (point3 1 -1 0)))
      (is (matrix/eq p3 (point3 5 -5 0)))
      (is (matrix/eq p4 (point3 15 0 7)))))

  (testing "Chained transformations must be applied in reverse order"
    (let [p (point3 1 0 1)
          A (matrix/rotation-x (/ pi 2))
          B (matrix/scaling 5 5 5)
          C (matrix/translation 10 5 7)
          T (reduce matrix/multiply [C B A])]
      (is (matrix/eq (matrix/multiply T p)
                     (point3 15 0 7))))))
  

(deftest chapter-five
  (testing "Creating and querying a ray"
    (let [o (point3 1 2 3)
          d (vector3 4 5 6)
          r (ray o d)]
      (is (= (origin r) o))
      (is (= (direction r) d))))

  (testing "Computing a point from a distance"
    (let [r (ray (point3 2 3 4) (vector3 1 0 0))]
      (is (= (position r 0) (point3 2 3 4)))
      (is (= (position r 1) (point3 3 3 4)))
      (is (= (position r -1) (point3 1 3 4)))
      (is (= (position r 2.5) (point3 4.5 3 4)))))

  (testing "A ray intersects a sphere at two points"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [4.0 6.0]))))

  (testing "A ray intersects a sphere at a tangent"
    (let [r (ray (point3 0 1 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [5.0 5.0]))))

  (testing "A ray misses a sphere"
    (let [r (ray (point3 0 2 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= xs []))))

  (testing "A ray originates inside a sphere"
    (let [r (ray (point3 0 0 0) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [-1.0 1.0]))))

  (testing "A sphere is behind a ray"
    (let [r (ray (point3 0 0 5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [-6.0 -4.0]))))

  (testing "An intersection encapsulates t and object"
    (let [s (sphere)
          i (intersection 3.5 s)]
      (is (= (:t i) 3.5))
      (is (= (:object i) s))))

  (testing "Aggregating intersections"
    (let [s (sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i1 i2)]
      (is (= (mapv :t xs) [1 2]))))

  (testing "Intersect sets the object on the intersection"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :object xs) [s s]))))

  (testing "The hit, when all intersections have positive t"
    (let [s (sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i1 i2)]
      (is (= (hit xs) i1))))

  (testing "The hit, when some intersections have negative t"
    (let [s (sphere)
          i1 (intersection -1 s)
          i2 (intersection 1 s)
          xs (intersections i1 i2)]
      (is (= (hit xs) i2))))

  (testing "The hit, when all intersections have negative t"
    (let [s (sphere)
          i1 (intersection -2 s)
          i2 (intersection -1 s)
          xs (intersections i1 i2)]
      (is (= (hit xs) nil))))

  (testing "The hit is always the lowest nonnegative intersection"
    (let [s (sphere)
          i1 (intersection 5 s)
          i2 (intersection 7 s)
          i3 (intersection -3 s)
          i4 (intersection 2 s)
          xs (intersections i1 i2 i3 i4)]
      (is (= (hit xs) i4))))

  (testing "Translating a ray"
    (let [r (ray (point3 1 2 3) (vector3 0 1 0))
          m (matrix/translation 3 4 5)
          r2 (transform r m)]
      (is (= (origin r2) (point3 4 6 8)))
      (is (= (direction r2) (vector3 0 1 0)))))

  (testing "Scaling a ray"
    (let [r (ray (point3 1 2 3) (vector3 0 1 0))
          m (matrix/scaling 2 3 4)
          r2 (transform r m)]
      (is (= (origin r2) (point3 2 6 12)))
      (is (= (direction r2) (vector3 0 3 0)))))

  (testing "A sphere's default transformation"
    (let [s (sphere)]
      (is (= (:transform s) matrix/id))))

  (testing "Changing a sphere's transformation"
    (let [s (sphere)
          t (matrix/translation 2 3 4)]
      (is (= (:transform (assoc s :transform t))
             t))))

  (testing "Intersecting a scaled sphere with a ray"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere :transform (matrix/scaling 2 2 2))
          xs (intersect s r)]
      (is (= (mapv :t xs) [3.0 7.0]))))

  (testing "Intersecting a translated sphere with a ray"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere :transform (matrix/translation 5 0 0))
          xs (intersect s r)]
      (is (= xs [])))))

(deftest chapter-six
  (testing "The normal on a sphere at a point on the x axis"
    (let [s (sphere)
          n (normal-at s (point3 1 0 0))]
      (is (matrix/eq n (vector3 1 0 0)))))

  (testing "The normal on a sphere at a point on the y axis"
    (let [s (sphere)
          n (normal-at s (point3 0 1 0))]
      (is (matrix/eq n (vector3 0 1 0)))))

  (testing "The normal on a sphere at a point on the z axis"
    (let [s (sphere)
          n (normal-at s (point3 0 0 1))]
      (is (matrix/eq n (vector3 0 0 1)))))

  (testing "The normal on a sphere at a nonaxial point"
    (let [s (sphere)
          v (/ (sqrt 3) 3)
          n (normal-at s (point3 v v v))]
      (is (matrix/eq n (vector3 v v v)))))

  (testing "The normal is a normalized vector"
    (let [s (sphere)
          v (/ (sqrt 3) 3)
          n (normal-at s (point3 v v v))]
      (is (matrix/eq n (tuple/normalize n)))))

  (testing "Computing the normal on a translated sphere"
    (let [s (sphere :transform (matrix/translation 0 1 0))
          n (normal-at s (point3 0 1.70711 -0.70711))]
      (is (matrix/eq n (vector3 0 0.70711 -0.70711)))))

  (testing "Computing the normal on a transformed sphere"
    (let [m (matrix/multiply (matrix/scaling 1 0.5 1)
                             (matrix/rotation-z (/ pi 5)))
          s (sphere :transform m)
          v (/ (sqrt 2) 2)
          n (normal-at s (point3 0 v (- v)))]
      (is (matrix/eq n (vector3 0 0.97014 -0.24254)))))

  (testing "A point light has a position and intensity"
    (let [intensity (rc/color 1 1 1)
          position  (point3 0 0 0)
          light     (->PointLight position intensity)]
      (is (= (:position light) position))
      (is (= (:intensity light) intensity))))

  (testing "The default material"
    (let [m (material)]
      (is (= (:color m) (rc/color 1 1 1)))
      (is (= (:ambient m) 0.1))
      (is (= (:diffuse m) 0.9))
      (is (= (:specular m) 0.9))
      (is (= (:shininess m) 200.0))))

  (testing "A sphere has a default material"
    (let [s (sphere)
          m (:material s)]
      (is (= m (material)))))

  (testing "A sphere may be assigned a material"
    (let [s (sphere)
          m (material)
          m (assoc m :ambient 1)
          s (assoc s :material m)]
      (is (= (:material s) m))))

  (let [m        (material)
        position (point3 0 0 0)]
    (testing "Lighting with the eye between the light and the surface"
      (let [eyev    (vector3 0 0 -1)
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 0 -10) (rc/color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (matrix/eq (rc/color 1.9 1.9 1.9) result))))
    (testing "Lighting with the eye between light and surface, eye offset 45°"
      (let [root    (/ (sqrt 2) 2)
            eyev    (vector3 0 root (- root))
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 0 -10) (rc/color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (matrix/eq (rc/color 1.0 1.0 1.0) result))))
    (testing "Lighting with the eye opposite surface, light offset 45°"
      (let [eyev    (vector3 0 0 -1)
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 10 -10) (rc/color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (matrix/eq (rc/color 0.7364 0.7364 0.7364) result))))
    (testing "Lighting with eye in the path of the reflection vector"
      (let [root    (/ (sqrt 2) 2)
            eyev    (vector3 0 (- root) (- root))
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 10 -10) (rc/color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (matrix/eq (rc/color 1.6364 1.6364 1.6364) result))))
    (testing "Lighting with the light behind the surface"
      (let [eyev    (vector3 0 0 -1)
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 0 10) (rc/color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (matrix/eq (rc/color 0.1 0.1 0.1) result))))))

(deftest chapter-seven
  (testing "Creating a world"
    (let [w (world)]
      (is (empty? (:objects w)))
      (is (nil? (:light w)))))

  (testing "The default world"
    (let [light (->PointLight (point3 -10 -10 -10) (rc/color 1 1 1))
          s1    (sphere
                 :material
                 (material
                  :color    (rc/color 0.8 1.0 0.6)
                  :diffuse  0.7
                  :specular 0.2))
          s2    (sphere :transform (matrix/scaling 0.5 0.5 0.5))
          w (default-world)]
      (is (= (:light w) light))
      (is (some (partial = s1) (:objects w)))
      (is (some (partial = s2) (:objects w)))))

  (testing "Intersect a world with a ray"
    (let [w  (default-world)
          r  (ray (point3 0 0 -5) (vector3 0 0 1))
          xs (intersect-world w r)]
      (is (== 4 (count xs) 4))
      (is (== 4 (:t (nth xs 0))))
      (is (== 4.5 (:t (nth xs 1))))
      (is (== 5.5 (:t (nth xs 2))))
      (is (== 6 (:t (nth xs 3))))))

  (testing "Precomputing the state of an intersection"
    (let [r     (ray (point3 0 0 -5) (vector3 0 0 1))
          shape (sphere)
          i     (intersection 4 shape)
          comps (prepare-computations i r)]
      (is (== (:t i) (:t comps)))
      (is (= (:object i) (:object comps)))
      (is (= (point3 0 0 -1) (:point comps)))
      (is (= (vector3 0 0 -1) (:eyev comps)))
      (is (= (vector3 0 0 -1) (:normalv comps)))))

  (testing "The hit, when an intersection occurs on the outside"
    (let [r     (ray (point3 0 0 -5) (vector3 0 0 1))
          shape (sphere)
          i     (intersection 4 shape)
          comps (prepare-computations i r)]
      (is (false? (:inside comps)))))

  (testing "The hit, when an intersection occurs on the outside"
    (let [r     (ray (point3 0 0 0) (vector3 0 0 1))
          shape (sphere)
          i     (intersection 1 shape)
          comps (prepare-computations i r)]
      (is (= (point3 0 0 1) (:point comps)))
      (is (= (vector3 0 0 -1) (:eyev comps)))
      (is (= (vector3 0 0 -1) (:normalv comps)))
      (is (true? (:inside comps)))))

  (testing "Shading an intersection"
    (let [w     (default-world)
          r     (ray (point3 0 0 -5) (vector3 0 0 1))
          shape (first (:objects w))
          i     (intersection 4 shape)
          comps (prepare-computations i r)
          c     (shade-hit w comps)]
      (is (matrix/eq (rc/color 0.38066 0.47583 0.2855) c))))

  (testing "The color when a ray misses"
    (let [w (default-world)
          r (ray (point3 0 0 -5) (vector3 0 1 0))
          c (color-at w r)]
      (is (matrix/eq (rc/color 0 0 0) c))))

  (testing "The color when a ray hits"
    (let [w (default-world)
          r (ray (point3 0 0 -5) (vector3 0 0 1))
          c (color-at w r)]
      (is (matrix/eq (rc/color 0.38066 0.47583 0.2855) c))))

  (testing "The transformation matrix for the default orientation"
    (let [from (point3 0 0 0)
          to   (point3 0 0 -1)
          up   (vector3 0 1 0)
          t    (view-transform from to up)]
      (is (matrix/eq matrix/id t))))

  (testing "A view transformation matrix looking in positive z direction"
    (let [from (point3 0 0 0)
          to   (point3 0 0 1)
          up   (vector3 0 1 0)
          t    (view-transform from to up)]
      (is (matrix/eq (matrix/scaling -1 1 -1) t))))

  (testing "The view transformation moves the world"
    (let [from (point3 0 0 8)
          to   (point3 0 0 0)
          up   (vector3 0 1 0)
          t    (view-transform from to up)]
      (is (matrix/eq (matrix/translation 0 0 -8) t))))

  (testing "An arbitrary view transformation"
    (let [from (point3 1 3 2)
          to   (point3 4 -2 8)
          up   (vector3 1 1 0)
          t    (view-transform from to up)]
      (is
       (matrix/eq
        [[-0.50709  0.50709  0.67612  -2.36643]
         [ 0.76772  0.60609  0.12122  -2.82843]
         [-0.35857  0.59761 -0.71714   0.00000]
         [ 0.00000  0.00000  0.00000   1.00000]]
        t)))))
