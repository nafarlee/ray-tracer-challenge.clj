(ns ray.core-test
  (:require
   pjstadig.humane-test-output
   [clojure.test :refer [deftest is testing]]
   [ray.transform :refer [view-transform]]
   [ray.world :refer [world color-at shade-hit default-world intersect-world]]
   [ray.material :refer [material]]
   [ray.light :refer [->PointLight lighting]]
   [ray.shape :refer [intersect
                      intersection
                      normal-at
                      prepare-computations
                      sphere]]
   [ray.ray :refer [ray]]
   [ray.math :refer [pi sqrt]]
   [ray.matrix :as matrix]
   [ray.tuple :as tuple]
   [ray.color :as rc]
   [ray.vector3 :refer [vector3]]
   [ray.point3 :refer [point3]]))

(pjstadig.humane-test-output/activate!)

(deftest chapter-five
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
