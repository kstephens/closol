(ns closol.mutate
  (:require
    [closol.random :refer :all]
    [closol.vfunc :refer :all]))

(def variables '[x y])
(def operators '[
      ;; UOPs
      [closol.vfunc/v-neg 1]
      [closol.vfunc/v-cos 1]
      [closol.vfunc/v-sin 1]
      ;;(v-acos 1)
      ;;(v-asin 1)
      [closol.vfunc/v-floor 1]
      ;;(v-real-part 1)
      ;;(v-imag-part 1)
      [closol.vfunc/v-magnitude 1]
      ;;(v-angle 1)
                        
      ;; BOPs
      [closol.vfunc/v-add 2]
      [closol.vfunc/v-sub 2]
      [closol.vfunc/v-mul 2]
      [closol.vfunc/v-div 2]
      [closol.vfunc/v-mod 2]
      [closol.vfunc/v-expt 2]
      [closol.vfunc/v-bit-xor 2]
      [closol.vfunc/v-bit-and 2]
      [closol.vfunc/v-bit-or  2]
      ;;(v-make-rectangular 2)
      ;;(v-make-polar 2)
      ;;(atan-safe 2)
                 
      ;; TOPs
      [closol.vfunc/v-if 3]
      [closol.vfunc/v-clamp 3]
      [closol.vfunc/v-lerp 3]
      [closol.vfunc/v-lerp-1 3]
      ])
(def operator-name-map
  (reduce #(assoc %1 (first %2) %2) {} operators))
(def operator-arity-map
  (reduce #(assoc %1 (second %2) (cons %2 (get %1 (second %2) []))) {} operators))

(defrecord Mutator [random variables operators])

(defn make-mutator [random]
  (Mutator. random variables operators))

(defn mutator-random    [cntx] (.random cntx))
(defn mutator-variables [cntx] (.variables cntx))
(defn mutator-operators [cntx] (.operators cntx))

(defn mutator-operator-def [cntx op]
  (get operator-name-map op))

(defn mutator-operators-same-arity [cntx op]
  (get operator-arity-map (mutator-operator-def op)))

(defn random-variable [cntx]
  (random-element (mutator-random cntx)
		  (mutator-variables cntx)))

(defn random-number [cntx]
  (- (* 20.0 ((mutator-random cntx))) 10.0))

(defn random-operator [cntx]
  (random-element (mutator-random cntx) 
		  (mutator-operators cntx)))

;; Generates a sequence n elements long of the result of proc.
(defn times [n proc]
  (map (fn [_] (proc)) (range 0 n)))

(defn expr-to-function
  [m expr]
  (binding [*ns* *ns*] (eval `(fn ~(mutator-variables m) ~expr))))

(defn random-expression
  "Generates a random expression of max depth."
  [cntx depth]
  (case ((mutator-random cntx) (+ depth 2))
    (0) (random-variable cntx)
    (1) (random-number cntx)
    (let [ro (random-operator cntx) depth (- depth 1)]
      (cons (first ro) 
        (times (second ro) 
          #(random-expression cntx depth))))))

(defn random-expression-of-depth
  "Generates a random expression of depth."
  [cntx depth]
  (if (> depth 0)
    (let [ro    (random-operator cntx)
          depth (- depth 1)]
      (cons (first ro)
        (times (second ro)
          #(random-expression-of-depth cntx depth))))
    (case ((mutator-random cntx) 2)
      (0) (random-variable cntx)
      (1) (random-number cntx))))

;; Generates a random expression with expr as a subexpression.
(defn random-subexpression-with [cntx expr]
  (let [ro (random-operator cntx)]
    (cons
      (first ro)
      (random-sequence (mutator-random cntx)
		   (cons expr
			 (times (- (second ro) 1) #(random-expression cntx 1))))
      )
    ))

(defn random-subexpression [cntx expr]
  (let [subexpressions expr]
    (random-element (mutator-random cntx) subexpressions)))

(declare mutate-expr-2)
(defn mutate-expr-1 [cntx expr root]
  ;;(display "  ")(write `(mutate-expr-1 cntx ,expr root))(newline)
  (let [result (mutate-expr-2 cntx expr root)]
    ;(display "  ")(write `(mutate-expr-1 cntx ,expr root))(newline)(display "  => ")(write result)(newline)
    result))
;;(define mutate-expr-1 mutate-expr-2)

(defn mutate-expr-2 [cntx expr root]
  (cond 
    (list? expr) 
    (case ((mutator-random cntx) 10)
      ;; generate a new random expression with expression
      (0) (random-subexpression-with cntx expr)
      ;; Select a random subexpression from the root.
      (1) (random-subexpression (mutator-random cntx) expr root)
      ;; Randomize order of arguments.
      (2)
      (cons (first expr)
        (random-sequence (mutator-random cntx) (rest expr)))
      ;; generate a new expression using the same operator
      (3)
      (cons (first expr)
              (map #(mutate-expr-1 cntx %1 root)
                   (rest expr)))
      ;; generator a new expression using a different operator with same arguments.
      (4)
      (cons (first (random-element 
		    (mutator-random cntx)
		    (mutator-operators-same-arity cntx (first expr))))
              (map #(mutate-expr-1 cntx %1 root) (rest expr)))
      ;; select the expression.
      :else expr)
    
    (symbol? expr)
    (case ((mutator-random cntx) 8)
      ;; replace with a new random expression.
      (0) (random-expression cntx 1)
      ;; generate a new random expression with expression
      (1) (random-subexpression-with cntx expr)
      ;; Select a random subexpression from the root.
      (2) (random-subexpression (mutator-random cntx) expr root)
      ;; Select a random variable.
      (3) (random-variable cntx)
      :else expr)
    
    :else 
    (case ((mutator-random cntx) 6)
      ;; replace with a new random expression.
      (0) (random-expression cntx 1)
      ;; generate a new random expression with expression
      (1) (random-subexpression-with cntx expr)
      ;; Select a random subexpression from the root.
      (2) (random-subexpression (mutator-random cntx) expr root)
      :else expr)
    ))

(defn mutate-expr [cntx expr]
  ;(write `(mutate-expr cntx ,expr))(newline)
  (let [result (mutate-expr-1 cntx expr expr)]
    ;;(write `((mutate-expr cntx ,expr) => ,result))(newline)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn breed-exprs-1 [cntx expr1 root1 expr2 root2]
  (cond
    (and (list? expr1) (list? expr2) (= (count expr1) (count expr2)))
    (case ((mutator-random cntx) 6)
      ;; Select an operator from either side and arguments from either side
      (0) 
      (cons
        (random-element (mutator-random cntx) [(first expr1) (rest expr2)])
        (random-element (mutator-random cntx) [(first expr1) (rest expr2)]))
      ;; Select an expression from either side and breed the arguments.
      (1)
      (cons
        (random-element (mutator-random cntx) [(first expr1) (first expr2)])
        (random-merge   (mutator-random cntx) (rest expr1) (rest expr2)))
      ;; Select an expression from either side of the same complexity.
      (2)
      (let [expr-root (random-element (mutator-random cntx)
                        [ (cons (expr1 root2) (cons expr2 root1))])]
        (random-subexpression (mutator-random cntx) (first expr-root) (first expr-root)))
      :else (random-element (mutator-random cntx) [expr1 expr2]))
    :else
     (random-element (mutator-random cntx) [expr1 expr2])))

