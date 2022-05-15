(ns ray.camera-test
  (:require
    [clojure.test :refer [deftest is testing]]
    pjstadig.humane-test-output
    [ray.vector3 :refer [vector3]]
    [ray.point3 :refer [point3]]
    [ray.camera :refer [camera pixel-size ray-for-pixel]]
    [ray.matrix :refer [eq id multiply rotation-y translation]]
    [ray.math :refer [float= pi sqrt]]))

(pjstadig.humane-test-output/activate!)

(deftest camera.feature
  (testing "Constructing a camera"
    (let [hsize         160
          vsize         120
          field-of-view (/ pi 2)
          c             (camera hsize vsize field-of-view)]
      (is (== 160 (:hsize c)))
      (is (== 120 (:vsize c)))
      (is (== (/ pi 2) (:field-of-view c)))
      (is (eq id (:transform c)))))

  (testing "The pixel size for a horizontal canvas"
    (let [c (camera 200 125 (/ pi 2))]
      (is (float= 0.01 (pixel-size c)))))

  (testing "The pixel size for a vertical canvas"
    (let [c (camera 125 200 (/ pi 2))]
      (is (float= 0.01 (pixel-size c)))))

  (testing "Constructing a ray through the center of the canvas"
    (let [c (camera 201 101 (/ pi 2))
          r (ray-for-pixel c 100 50)]
      (is (eq (point3 0 0 0) (:origin r)))
      (is (eq (vector3 0 0 -1) (:direction r)))))

  (testing "Constructing a ray through the corner of the canvas"
    (let [c (camera 201 101 (/ pi 2))
          r (ray-for-pixel c 0 0)]
      (is (eq (point3 0 0 0) (:origin r)))
      (is (eq (vector3 0.66519 0.33259 -0.66851) (:direction r)))))

  (testing "Constructing a ray when the camera is transformed"
    (let [c (camera
             201
             101
             (/ pi 2)
             (multiply (rotation-y (/ pi 4)) (translation 0 -2 5)))
          r (ray-for-pixel c 100 50)]
      (is (eq (point3 0 2 -5) (:origin r)))
      (is (eq (vector3 (/ (sqrt 2) 2) 0 (- (/ (sqrt 2) 2)))
              (:direction r))))))
