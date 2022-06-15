(ns ray.canvas-test
  (:require
   [clojure.string :refer [split-lines join ends-with?]]
   [clojure.test :refer [deftest is testing]]
   pjstadig.humane-test-output
   [ray.string :refer [$]]
   [ray.ppm :refer [canvas->ppm]]
   [ray.matrix :refer [eq]]
   [ray.color :refer [color]]
   [ray.canvas :refer [canvas write-pixel pixel-at]]))

(pjstadig.humane-test-output/activate!)

(deftest canvas.feature
  (testing "Creating a canvas"
    (let [c (canvas 10 20)]
      (is (== (:width c) 10))
      (is (== (:height c) 20))
      (is (every? (partial eq (color 0 0 0))
                  (:pixels c)))))

  (testing "Writing pixels to a canvas"
    (let [c (canvas 10 20)
          red (color 1 0 0)]
      (is (eq red
                     (-> c
                         (write-pixel 2 3 red)
                         (pixel-at 2 3))))))

  (testing "Constructing the PPM header"
    (let [c (canvas 5 3)
          ppm (canvas->ppm c)
          header (->> ppm (split-lines) (take 3) (join "\n"))]
      (is (= header ($ "
                          P3
                          5 3
                          255
                          ")))))

  (testing "Constructing the PPM pixel data"
    (let [c (canvas 5 3)
          c1 (color 1.5 0 0)
          c2 (color 0 0.5 0)
          c3 (color -0.5 0 1)]
      (is (= ($ "
                   255 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 128 0 0 0 0 0 0 0
                   0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
                   ")
             (as-> c %
                   (write-pixel % 0 0 c1)
                   (write-pixel % 2 1 c2)
                   (write-pixel % 4 2 c3)
                   (canvas->ppm %)
                   (split-lines %)
                   (drop 3 %)
                   (take 3 %)
                   (join "\n" %))))))

  (testing "Splitting long lines in PPM files"
    (let [c (canvas 10 2)
          color (color 1 0.8 0.6)]
      (is (= ($ "
                   255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
                   153 255 204 153 255 204 153 255 204 153 255 204 153
                   255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
                   153 255 204 153 255 204 153 255 204 153 255 204 153
                   ")
             (as-> c $
                   (reduce (fn [can [x y]] (write-pixel can x y color))
                           $
                           (for [x (range 10), y (range 2)] [x y]))
                   (canvas->ppm $)
                   (split-lines $)
                   (drop 3 $)
                   (take 4 $)
                   (join "\n" $))))))

  (testing "PPM files are terminated by a newline character"
    (let [c (canvas 5 3)]
      (is (ends-with? (canvas->ppm c)
                      "\n")))))
