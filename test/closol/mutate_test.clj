(ns closol.mutate-test
  (:require
    [clojure.pprint :refer :all]
    [clojure.test :refer :all]
    [closol.expr :refer :all]
    [closol.mutate :refer :all]
    [closol.random :refer :all]
    [closol.matrix :refer :all]
    [closol.sample :refer [sample]]
    [clojure.string :as string]
    [clojure.java.io :refer :all])
  (:import
    (java.io File))
  )

(def current-pid
  "Get current process PID"
  (memoize
   (fn []
     (-> (java.lang.management.ManagementFactory/getRuntimeMXBean)
         (.getName)
         (string/split #"@")
         (first)))))

(defmacro with-out-file
  "Evaluates exprs in a context in which *out* is bound to a file.
Returns the last value from the body."
  [file & body]
  `(with-open [wrtr# (writer ~file)]
     (binding [*out* wrtr#]
       ~@body)))

(defn make-image-from-seed
  [seed]
  (let [ m          (make-mutator (make-random seed))
         file_png   (str "tmp/png/test" seed ".png")
         file_expr  (str "tmp/expr/test" seed ".expr") ]
    (if (.exists (File. file_expr))
      (do
        (println "  ### File " file_expr " already exists.")
        false)
      (do
        (println (str "  ### Creating " file_png " from:"))
        (dosync
          (let [ e  (random-expression m 10)
                 e2 (finish-expression e)
                 f  (expr-to-function m e2) ]
            (with-out-file file_expr
              (println [:seed seed])
              (pprint  [:expr e2])
              #_ (println [:operators (.operators m)]))
            (println (slurp file_expr))
            (if (not (seq? e2))
              (do
                (println "  ### Image for seed" seed "is linear!")
                false)
              (let [m (matrix-graymap
                       (matrix-fix-float
                        (matrix-fxy 512 512 -10.0 10.0 -10.0 10.0 f))) ]
                (if (matrix-zero? m)
                  (do
                    (println "  ### Image of seed" seed "is all zeros!")
                    false)
                  (do
                    (image-to-file (matrix-image m) file_png)
                    file_png)
                  )))))))))

(def rnd (make-random (Integer. (current-pid))))

(deftest generate-image-test
  (testing "random-expression image"
    (doall
      (filter make-image-from-seed
              (map (fn [x] (dosync (rnd 10000000)))
                   (take 100 (range)))))
    ))
