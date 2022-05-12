(ns ray.core-test
  (:require
   pjstadig.humane-test-output
   [clojure.test :refer [deftest is testing]]
   [ray.transform :refer [view-transform]]
   [ray.matrix :as matrix]
   [ray.vector3 :refer [vector3]]
   [ray.point3 :refer [point3]]))

(pjstadig.humane-test-output/activate!)

(deftest chapter-seven
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
