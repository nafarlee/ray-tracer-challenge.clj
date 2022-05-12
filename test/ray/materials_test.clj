(ns ray.materials-test
  (:require
   pjstadig.humane-test-output
   [clojure.test :refer [deftest is testing]]
   [ray.math :refer [sqrt]]
   [ray.matrix :refer [eq]]
   [ray.light :refer [->PointLight lighting]]
   [ray.vector3 :refer [vector3]]
   [ray.point3 :refer [point3]]
   [ray.color :refer [color]]
   [ray.material :refer [material]]))

(pjstadig.humane-test-output/activate!)

(deftest materials.feature
  (testing "The default material"
    (let [m (material)]
      (is (= (:color m) (color 1 1 1)))
      (is (= (:ambient m) 0.1))
      (is (= (:diffuse m) 0.9))
      (is (= (:specular m) 0.9))
      (is (= (:shininess m) 200.0))))

  (let [m        (material)
        position (point3 0 0 0)]
    (testing "Lighting with the eye between the light and the surface"
      (let [eyev    (vector3 0 0 -1)
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 0 -10) (color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (eq (color 1.9 1.9 1.9) result))))
    (testing "Lighting with the eye between light and surface, eye offset 45°"
      (let [root    (/ (sqrt 2) 2)
            eyev    (vector3 0 root (- root))
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 0 -10) (color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (eq (color 1.0 1.0 1.0) result))))
    (testing "Lighting with the eye opposite surface, light offset 45°"
      (let [eyev    (vector3 0 0 -1)
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 10 -10) (color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (eq (color 0.7364 0.7364 0.7364) result))))
    (testing "Lighting with eye in the path of the reflection vector"
      (let [root    (/ (sqrt 2) 2)
            eyev    (vector3 0 (- root) (- root))
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 10 -10) (color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (eq (color 1.6364 1.6364 1.6364) result))))
    (testing "Lighting with the light behind the surface"
      (let [eyev    (vector3 0 0 -1)
            normalv (vector3 0 0 -1)
            light   (->PointLight (point3 0 0 10) (color 1 1 1))
            result  (lighting m light position eyev normalv)]
        (is (eq (color 0.1 0.1 0.1) result))))))
