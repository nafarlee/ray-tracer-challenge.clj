(ns ray.spheres-test
  (:require
   [clojure.test :refer [deftest is testing]]
   pjstadig.humane-test-output
   [ray.material :refer [material]]
   [ray.ray :refer [ray]]
   [ray.math :refer [pi sqrt]]
   [ray.matrix :refer [translation eq id multiply scaling rotation-z]]
   [ray.point3 :refer [point3]]
   [ray.vector3 :refer [vector3]]
   [ray.tuple :refer [normalize]]
   [ray.shape :refer [intersect normal-at sphere]]))

(pjstadig.humane-test-output/activate!)

(deftest spheres.feature
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

  (testing "A sphere's default transformation"
    (let [s (sphere)]
      (is (= (:transform s) id))))

  (testing "Changing a sphere's transformation"
    (let [s (sphere)
          t (translation 2 3 4)]
      (is (= (:transform (assoc s :transform t))
             t))))

  (testing "Intersecting a scaled sphere with a ray"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere :transform (scaling 2 2 2))
          xs (intersect s r)]
      (is (= (mapv :t xs) [3.0 7.0]))))

  (testing "Intersecting a translated sphere with a ray"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere :transform (translation 5 0 0))
          xs (intersect s r)]
      (is (= xs []))))

  (testing "The normal on a sphere at a point on the x axis"
    (let [s (sphere)
          n (normal-at s (point3 1 0 0))]
      (is (eq n (vector3 1 0 0)))))

  (testing "The normal on a sphere at a point on the y axis"
    (let [s (sphere)
          n (normal-at s (point3 0 1 0))]
      (is (eq n (vector3 0 1 0)))))

  (testing "The normal on a sphere at a point on the z axis"
    (let [s (sphere)
          n (normal-at s (point3 0 0 1))]
      (is (eq n (vector3 0 0 1)))))

  (testing "The normal on a sphere at a nonaxial point"
    (let [s (sphere)
          v (/ (sqrt 3) 3)
          n (normal-at s (point3 v v v))]
      (is (eq n (vector3 v v v)))))

  (testing "The normal is a normalized vector"
    (let [s (sphere)
          v (/ (sqrt 3) 3)
          n (normal-at s (point3 v v v))]
      (is (eq n (normalize n)))))

  (testing "Computing the normal on a translated sphere"
    (let [s (sphere :transform (translation 0 1 0))
          n (normal-at s (point3 0 1.70711 -0.70711))]
      (is (eq n (vector3 0 0.70711 -0.70711)))))

  (testing "Computing the normal on a transformed sphere"
    (let [m (multiply (scaling 1 0.5 1)
                      (rotation-z (/ pi 5)))
          s (sphere :transform m)
          v (/ (sqrt 2) 2)
          n (normal-at s (point3 0 v (- v)))]
      (is (eq n (vector3 0 0.97014 -0.24254)))))

  (testing "A sphere has a default material"
    (let [s (sphere)
          m (:material s)]
      (is (= m (material)))))

  (testing "A sphere may be assigned a material"
    (let [s (sphere)
          m (material)
          m (assoc m :ambient 1)
          s (assoc s :material m)]
      (is (= (:material s) m)))))
