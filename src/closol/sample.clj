(ns closol.sample
  (:require
   [closol.v :refer [v-lerp]]
   [closol.random2 :refer [make-random rnd]]))

(defonce ^:private rand-state (make-random))
(def ^:private rand (rnd rand-state))

(def ^:private add +)
(def ^:private div /)

(defn ^:private sum
  [s]
  (reduce add s))
(defn ^:private avg
  [n s]
  (div (sum (take n s)) (double n)))

(defn ^:private rand-sample [f x0 x1 y0 y1]
  (fn []
    (let [x (v-lerp (rand) x0 x1)
          y (v-lerp (rand) y1 y1)]
      (f x y))))

(def n-samples 16)
(defn sample [f]
  (fn [x0 x1 y0 y1]
    (let [fs (rand-sample f x0 x1 y0 y1)]
      (avg n-samples (repeatedly fs)))))
