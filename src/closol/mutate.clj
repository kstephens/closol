(ns closol.mutate
  (:require
    [clojure.walk  :refer :all]
    [closol.match  :refer :all]
    [closol.expr   :refer :all]
    [closol.random :refer :all]
    [closol.vfunc  :refer :all]))

(def variables '[x y])
(def operators '[
      ;; UOPs
      [closol.vfunc/v-neg 1]
      [closol.vfunc/v-cos 1]
      [closol.vfunc/v-sin 1]
      [closol.vfunc/v-acos 1]
      [closol.vfunc/v-asin 1]
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
      ;; [closol.vfunc/v-atan 2]
      [closol.vfunc/v-bit-xor 2]
      [closol.vfunc/v-bit-and 2]
      [closol.vfunc/v-bit-or  2]
      [closol.vfunc/v-bit-shift-left  2]
      [closol.vfunc/v-bit-shift-right 2]
      ;;(v-make-rectangular 2)
      ;;(v-make-polar 2)
                 
      ;; TOPs
      [closol.vfunc/v-if 3]
      [closol.vfunc/v-clamp 3]
      [closol.vfunc/v-lerp 3]
      [closol.vfunc/v-lerp-1 3]
      ])

(defrecord Mutator [random variables operators
                     operator-name-map operator-arity-map])

(defn make-mutator [random]
  (Mutator. random variables operators
    (reduce #(assoc %1 (first %2) %2) {} operators)
    (reduce #(assoc %1 (second %2) (cons %2 (get %1 (second %2) []))) {} operators)
    ))

(defn mutator-operator-def [cntx op]
  (get (.operator-name-map cntx) op))
(defn mutator-operators-same-arity [cntx op]
  (get (.operator-arity-map cntx) (mutator-operator-def op)))

(defn random-variable [cntx]
  (random-element (.random cntx) (.variables cntx)))
(defn random-number [cntx]
  (- (* 20.0 ((.random cntx))) 10.0))
(defn random-operator [cntx]
  (random-element (.random cntx) (.operators cntx)))

;; Generates a sequence n elements long of the result of proc.
(defn times [n proc]
  (map (fn [_] (proc)) (range 0 n)))

(defn expr-to-function
  [m expr]
  (binding [*ns* *ns*] (eval `(fn ~(.variables m) ~expr))))

(defn random-expression
  "Generates a random expression of max depth."
  [cntx depth]
  (case ((.random cntx) (+ depth 2))
    (0) (random-variable cntx)
    (1) (random-number cntx)
    (let [ ro    (random-operator cntx)
           depth (- depth 1) ]
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
    (case ((.random cntx) 2)
      (0) (random-variable cntx)
      (1) (random-number cntx))))

(defn random-subexpression-of-complexity
  "Selects a random subexpression from root expression of the same complexity as expr.
Returns expr if one of similar complexity cannot be found."
  [r expr root]
  (let [ expr-c     (expression-complexity expr)
         valid-exps (filter #(= (expression-complexity %1) expr-c)
                      (subexpressions root))]
    (if (empty? valid-exps) expr (random-element r valid-exps))))

;; Generates a random expression with expr as a subexpression.
(defn random-subexpression-with [cntx expr]
  (let [ro (random-operator cntx)]
    (cons
      (first ro)
      (random-sequence (.random cntx)
        (cons expr (times (- (second ro) 1) #(random-expression cntx 1)))))))

(defn random-subexpression [cntx expr]
  (random-element (.random cntx) (subexpressions expr)))

(declare mutate-expr-2)
(defn mutate-expr-1 [cntx expr root]
  ;;(display "  ")(write `(mutate-expr-1 cntx ,expr root))(newline)
  (let [result (mutate-expr-2 cntx expr root)]
    ;(display "  ")(write `(mutate-expr-1 cntx ,expr root))(newline)(display "  => ")(write result)(newline)
    result))
;;(define mutate-expr-1 mutate-expr-2)

(defn mutate-expr-3 [cntx expr root]
  (case ((.random cntx) 3)
    ;; Replace with a new random expression.
    (0) (random-expression cntx 1)
    ;; Generate a new random expression with expression.
    (1) (random-subexpression-with cntx expr)
    ;; Select a random subexpression from the root.
    (2) (random-subexpression cntx root)
    ;; Default: Select the expression.
    expr))

(defn mutate-expr-2 [cntx expr root]
  (cond 
    (list? expr) 
    (case ((.random cntx) 7)
      ;; Replace with a new random expression.
      (0) (random-expression cntx 1)
      ;; Generate a new random expression with expression.
      (1) (random-subexpression-with cntx expr)
      ;; Select a random subexpression from the root.
      (2) (random-subexpression cntx root)
      ;; Randomize order of arguments.
      (3)
      (cons (first expr)
        (random-sequence (.random cntx) (rest expr)))
      ;; Generate a new expression using the same operator.
      (4)
      (cons (first expr)
        (map #(mutate-expr-2 cntx %1 root) (rest expr)))
      ;; Generate a new expression using a different operator with same arguments.
      (5)
      (cons (first (random-element (.random cntx)
                     (mutator-operators-same-arity cntx (first expr))))
        (rest expr))
      ;; Generate a new expression of the same depth.
      (6)
      (random-expression-of-depth cntx (expression-depth expr))
      ;; Default: Select the expression.
      expr)
    
    :else 
    (mutate-expr-3 cntx expr root)))

(defn mutate-expr [cntx expr]
  ;(write `(mutate-expr cntx ,expr))(newline)
  (let [result (mutate-expr-1 cntx expr expr)]
    ;;(write `((mutate-expr cntx ,expr) => ,result))(newline)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn breed-exprs-1 [cntx expr1 root1 expr2 root2]
  (cond
    (and (list? expr1) (list? expr2) (= (count expr1) (count expr2)))
    (case ((.random cntx) 6)
      ;; Select an operator from either side and arguments from either side
      (0) 
      (cons
        (random-element (.random cntx) [(first expr1) (first expr2)])
        (random-element (.random cntx) [(rest expr1) (rest expr2)]))
      ;; Select an expression from either side and breed the arguments.
      (1)
      (cons
        (random-element (.random cntx) [(first expr1) (first expr2)])
        (random-merge   (.random cntx) (rest expr1) (rest expr2)))
      ;; Select an expression from either side of the same complexity.
      (2)
      (let [ expr-root (random-element (.random cntx) [ [expr1 root2] [expr2 root2] ]) ]
        (random-subexpression-of-complexity (.random cntx) (first expr-root) (second expr-root)))

      (random-element (.random cntx) [expr1 expr2]))
    :else
    (random-element (.random cntx) [expr1 expr2])))

(defn mix-expr [cntx expr1 expr2]
  (let [ expr-list1 (if (seq? expr1) (rest expr1) [ expr1 ])
         expr-list2 (if (seq? expr2) (rest expr2) [ expr2 ])
         exprs      (filter seq? [expr1 expr2]) ]
    (cond
      (empty? exprs) (random-element (.random cntx) (concat expr-list1 expr-list2))
      :else
      (let [ expr (random-element (.random cntx) exprs)
             args (random-sequence (.random cntx) (concat expr-list1 expr-list2)) ]
        (cons (first expr) (take (count (rest expr)) args)))
      )))

#_ (defn breed-exprs [expr1 expr2]
     (breed-exprs-2 expr1 expr1 expr2 expr2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn const-fold-if
  [expr]
  (if (and (seq? expr) (= `v-if (first expr)) (number? (second expr)))
    (apply v-if (rest expr))
    expr))

(def finish-expression
  (while-change-func
    (fn [expr]
      (walk const-fold-if const-fold-if (constant-fold expr)))))
