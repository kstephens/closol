(ns closol.matrix-test
  (:require
    [clojure.test :refer :all]
    [closol.matrix :refer :all]))

(deftest matrix-test
  (testing "matrix"
    (let [m (matrix 3 4 #(+ %1 (* %2 3) 1))]
      (is (= (matrix-data m)
            [ [1 2 3]
              [4 5 6]
              [7 8 9]
              [10 11 12] ]))
      (is (= (matrix-get m 0 0) 1))
      (is (= (matrix-get m 1 1) 5))
      (is (= (matrix-min-max m) [1 12]))
      (is (= (matrix-data (matrix-range m 0.0 1.0))
            [ [0.0 0.09090909090909091 0.18181818181818182]
              [0.2727272727272727 0.36363636363636365 0.45454545454545453]
              [0.5454545454545454 0.6363636363636364 0.7272727272727273]
              [0.8181818181818182 0.9090909090909091 1.0]]))
      (is (= (matrix-data (matrix-graymap m))
            [[0 23 46] [69 93 116] [139 162 186] [209 232 255]]
            ))

      (is (image-to-file (matrix-image (matrix-graymap m)) "test.png"))
      )))


