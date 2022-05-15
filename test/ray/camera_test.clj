(ns ray.camera-test
  (:require
    [clojure.test :refer [deftest is testing]]
    pjstadig.humane-test-output
    [ray.camera :refer [camera]]
    [ray.matrix :refer [id]]
    [ray.math :refer [pi]]))

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
      (is (= id (:transform c)))))

  (testing "The pixel size for a horizontal canvas"
    (let [c (camera 200 125 (/ pi 2))]
      (is (== 0.01 (:pixel-size c)))))

  (testing "The pixel size for a vertical canvas"
    (let [c (camera 125 200 (/ pi 2))]
      (is (== 0.01 (:pixel-size c))))))
