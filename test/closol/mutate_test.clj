(ns closol.mutate-test
  (:require
    [clojure.test :refer :all]
    [closol.mutate :refer :all]
    [closol.random :refer :all]))

(deftest random-expression-of-depth-test
  (testing "random-expression-of-depth"
    (let [m (make-mutator (make-random 1))]
      (dosync
        (is (= (random-expression-of-depth m 0)
              'x))
        (is (= (random-expression-of-depth m 1)
              '(v-floor -5.195632175255836)))
        (is (= (random-expression-of-depth m 2)
              '(v-add (v-div 4.79283653600204 -7.426849507489303) (v-lerp-1 -5.544365499004716 -9.42147682320664 -4.191442444516485))))
        (is (= (random-expression-of-depth m 3)
              '(v-sub (v-add (v-mul 1.0270724870546601 9.886328269194081) (v-div x x)) (v-mul (v-sub -7.9016014806822295 y) (v-magnitude y)))))
        (is (= (random-expression-of-depth m 4)
              '(v-sin (v-add (v-lerp-1 (v-sub -0.3776215509956984 y) (v-mul 0.17040577157121106 y) (v-sub -5.450294863818382 -3.463021240569434)) (v-mul (v-cos -7.346355462375779) (v-cos 5.1152809405462065))))))
        ))))

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
              '(v-if y x x)))
        (is (= (random-expression m 1)
              'y))
        (is (= (random-expression m 2)
              '(v-lerp -6.356424058088464 x (v-expt -6.955565240095595 y))))
        (is (= (random-expression m 2)
              'y))
        (is (= (random-expression m 2)
              '(v-sub -8.08166779859427 (v-clamp 4.04579252122894 x y))))
        (is (= (random-expression m 3)
              '(v-magnitude 3.8950664087969535)))
        (is (= (random-expression m 3)
              'y))
        (is (= (random-expression m 3)
              '(v-expt x y)))
        (is (= (random-expression m 4)
              '(v-floor (v-mod 8.476726147616851 x))))
        (is (= (random-expression m 10)
              '(v-sin x)))
        (is (= (random-expression m 10)
              '(v-cos (v-lerp-1 (v-clamp (v-neg (v-mul y x)) (v-add (v-clamp (v-magnitude -4.279195196347489) (v-div (v-lerp (v-div -4.546846212878365 y) (v-neg (v-expt x (v-if y -4.266569920110729 6.245522001183868))) 6.349502082265875) (v-div (v-mod y (v-sub x (v-clamp y x -0.7087100340577095))) 9.70497196699974)) (v-if (v-lerp-1 (v-sin x) (v-add (v-expt -0.9122427100073232 (v-magnitude 9.861323913403432)) (v-div (v-clamp -4.797732419020396 5.581904539029141 x) -4.665644799990787)) (v-add x 3.165271579633105)) (v-mod (v-floor (v-magnitude (v-cos y))) (v-mul -5.463839164206022 x)) (v-neg -7.649324857779444))) (v-sin (v-lerp-1 y 2.5799390164234346 (v-sin (v-if y x (v-expt y (v-sin y))))))) (v-magnitude (v-lerp-1 (v-neg 3.394156655048823) (v-expt (v-add 9.247128576705375 5.492175138081556) 8.213321313664764) (v-mod y y)))) x (v-div (v-magnitude (v-sub (v-cos x) (v-cos (v-lerp (v-neg (v-mul y x)) x (v-cos 4.539305902282891))))) (v-neg (v-expt y (v-lerp-1 (v-cos x) (v-div x -4.154646905155013) (v-add -7.5411362692834505 (v-add (v-if -2.657744509933396 x (v-div x 9.6248552462203)) -8.1497167585064))))))))))
        )
      )))

