(ns closol.mutate-test
  (:require
    [clojure.pprint :refer :all]
    [clojure.test :refer :all]
    [closol.expr :refer :all]
    [closol.mutate :refer :all]
    [closol.random :refer :all]
    [closol.matrix :refer :all]
    [clojure.java.io :refer :all])
  (:import
    (java.io File))
  )

(defmacro with-out-file
  "Evaluates exprs in a context in which *out* is bound to a file.
Returns the last value from the body."
  [file & body]
  `(with-open [wrtr# (writer ~file)]
     (binding [*out* wrtr#]
       ~@body)))

#_
(deftest random-expression-of-depth-test
  (testing "random-expression-of-depth"
    (let [m (make-mutator (make-random 1))]
      (dosync
        (is (= (random-expression-of-depth m 0)
              'x))
        (is (= (random-expression-of-depth m 1)
              '(closol.vfunc/v-floor -5.195632175255836)))
        (is (= (random-expression-of-depth m 2)
              '(closol.vfunc/v-add (closol.vfunc/v-div 4.79283653600204 -7.426849507489303) (closol.vfunc/v-lerp-1 -5.544365499004716 -9.42147682320664 -4.191442444516485))))
        (is (= (random-expression-of-depth m 3)
              '(closol.vfunc/v-sub (closol.vfunc/v-add (closol.vfunc/v-mul 1.0270724870546601 9.886328269194081) (closol.vfunc/v-div x x)) (closol.vfunc/v-mul (closol.vfunc/v-sub -7.9016014806822295 y) (closol.vfunc/v-magnitude y)))))
        (is (= (random-expression-of-depth m 4)
              '(closol.vfunc/v-sin (closol.vfunc/v-add (closol.vfunc/v-lerp-1 (closol.vfunc/v-sub -0.3776215509956984 y) (closol.vfunc/v-mul 0.17040577157121106 y) (closol.vfunc/v-sub -5.450294863818382 -3.463021240569434)) (closol.vfunc/v-mul (closol.vfunc/v-cos -7.346355462375779) (closol.vfunc/v-cos 5.1152809405462065))))))
        ))))

#_
(deftest random-expression-test
  (testing "random-expression"
    (let [m (make-mutator (make-random 2))]
      (dosync
        (is (= (random-expression m 0)
              'x))
        (is (= (random-expression m 0)
              'x))
        (is (= (random-expression m 0)
              'y))
        (is (= (random-expression m 1)
              'y))
        (is (= (random-expression m 1)
              '(closol.vfunc/v-if y x x)))
        (is (= (random-expression m 1)
              'y))
        (is (= (random-expression m 2)
              '(closol.vfunc/v-lerp -6.356424058088464 x (closol.vfunc/v-expt -6.955565240095595 y))))
        (is (= (random-expression m 2)
              'y))
        (is (= (random-expression m 2)
              '(closol.vfunc/v-sub -8.08166779859427 (closol.vfunc/v-clamp 4.04579252122894 x y))))
        (is (= (random-expression m 3)
              '(closol.vfunc/v-magnitude 3.8950664087969535)))
        (is (= (random-expression m 3)
              'y))
        (is (= (random-expression m 3)
              '(closol.vfunc/v-expt x y)))
        (is (= (random-expression m 4)
              '(closol.vfunc/v-floor (closol.vfunc/v-mod 8.476726147616851 x))))
        (is (= (random-expression m 10)
              '(closol.vfunc/v-sin x)))
        (is (= (random-expression m 10)
              '(closol.vfunc/v-cos (closol.vfunc/v-lerp-1 (closol.vfunc/v-clamp (closol.vfunc/v-neg (closol.vfunc/v-mul y x)) (closol.vfunc/v-add (closol.vfunc/v-clamp (closol.vfunc/v-magnitude -4.279195196347489) (closol.vfunc/v-div (closol.vfunc/v-lerp (closol.vfunc/v-div -4.546846212878365 y) (closol.vfunc/v-neg (closol.vfunc/v-expt x (closol.vfunc/v-if y -4.266569920110729 6.245522001183868))) 6.349502082265875) (closol.vfunc/v-div (closol.vfunc/v-mod y (closol.vfunc/v-sub x (closol.vfunc/v-clamp y x -0.7087100340577095))) 9.70497196699974)) (closol.vfunc/v-if (closol.vfunc/v-lerp-1 (closol.vfunc/v-sin x) (closol.vfunc/v-add (closol.vfunc/v-expt -0.9122427100073232 (closol.vfunc/v-magnitude 9.861323913403432)) (closol.vfunc/v-div (closol.vfunc/v-clamp -4.797732419020396 5.581904539029141 x) -4.665644799990787)) (closol.vfunc/v-add x 3.165271579633105)) (closol.vfunc/v-mod (closol.vfunc/v-floor (closol.vfunc/v-magnitude (closol.vfunc/v-cos y))) (closol.vfunc/v-mul -5.463839164206022 x)) (closol.vfunc/v-neg -7.649324857779444))) (closol.vfunc/v-sin (closol.vfunc/v-lerp-1 y 2.5799390164234346 (closol.vfunc/v-sin (closol.vfunc/v-if y x (closol.vfunc/v-expt y (closol.vfunc/v-sin y))))))) (closol.vfunc/v-magnitude (closol.vfunc/v-lerp-1 (closol.vfunc/v-neg 3.394156655048823) (closol.vfunc/v-expt (closol.vfunc/v-add 9.247128576705375 5.492175138081556) 8.213321313664764) (closol.vfunc/v-mod y y)))) x (closol.vfunc/v-div (closol.vfunc/v-magnitude (closol.vfunc/v-sub (closol.vfunc/v-cos x) (closol.vfunc/v-cos (closol.vfunc/v-lerp (closol.vfunc/v-neg (closol.vfunc/v-mul y x)) x (closol.vfunc/v-cos 4.539305902282891))))) (closol.vfunc/v-neg (closol.vfunc/v-expt y (closol.vfunc/v-lerp-1 (closol.vfunc/v-cos x) (closol.vfunc/v-div x -4.154646905155013) (closol.vfunc/v-add -7.5411362692834505 (closol.vfunc/v-add (closol.vfunc/v-if -2.657744509933396 x (closol.vfunc/v-div x 9.6248552462203)) -8.1497167585064))))))))))
        )
      )))

(defn make-image-from-seed
  [seed]
  (let [ m          (make-mutator (make-random seed))
         file_png   (str "tmp/test" seed ".png")
         file_expr  (str "tmp/test" seed ".expr") ]
    (if (.exists (File. file_expr))
      (do
        (println "  ### File " file_expr " already exists.")
        false)
      (do
        (println (str "\n  ### Creating " file_png " from:"))
        (dosync
          (let [ e  (random-expression m 10)
                 e2 (finish-expression e)
                 f  (expr-to-function m e2) ]
            (with-out-file file_expr (pprint e2))
            (println (slurp file_expr))
            (let [ fxy (matrix-graymap (matrix-fxy 512 512 -10.0 10.0 -10.0 10.0 f)) ]
              (if (matrix-zero? fxy)
                (do
                  (println "  ### Image is all zeros!")
                  false)
                (do
                  (image-to-file (matrix-image fxy) file_png)
                  file_png)
                ))))))))

(deftest generate-image-test
  (testing "random-expression image"
    (doall (take 10 (filter make-image-from-seed (range))))
    ))
