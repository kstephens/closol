#!r6rs
; R6RS main
(import 
 (mutate v-func)
 (mutate matrix)
 (mutate random)
 (mutate expr)
 (mutate sequence)
 (mutate csee)
 (rnrs)
 (rnrs eval (6)) ; environment eval
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
; For PLT
(require plot)
(require htdp/image)
; (require (planet wmfarr/simple-matrix:1:1/matrix))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mutator-context)
  (list->vector
    `(mutator-context
       variables (x y)
       operators (
                  ; UOPs
		  (v-neg 1)
                  (v-cos 1)
                  (v-sin 1)
                  ;(acos 1)
                  ;(asin 1)
                  (v-floor 1)
                  ;(real-part 1)
                  ;(imag-part 1)
                  (v-magnitude 1)
                  ;(v-angle 1)
                  
                  
                  ; BOPs
                  (v-+ 2)
                  (v-- 2)
                  (v-* 2)
                  (v-/ 2)
                  (v-mod 2)
                  (v-expt 2)
                  ;(v-make-rectangular 2)
                  ;(v-make-polar 2)
                  ;(atan-safe 2)
                 
                  ; TOPs
                  (v-if 3)
                  (v-clamp 3)
		  (v-lerp 3)
		  (v-lerp-1 3)
		  )
       random ,my-random
       )
    ))

(define (mutator-context-variables cntx)
  (vector-ref cntx 2))

(define (mutator-context-operators cntx)
  (vector-ref cntx 4))

(define (mutator-context-random cntx)
  (vector-ref cntx 6))

(define (mutator-context-operator-def cntx op)
  ;(write `(m-c-o-d ,op))(newline)
  (car (select-if (lambda (x) (eq? op (car x)))
                  (mutator-context-operators cntx))
       ))

(define (mutator-context-operators-same-arity cntx op)
  ;(write `(m-c-o-s-a ,op))(newline)
  (let ((op-def (mutator-context-operator-def cntx op)))
    (select-if (lambda (x)
                 (= (cadr op-def) (cadr x)))
               (mutator-context-operators cntx))
    ))


(define (random-variable cntx)
  (random-element (mutator-context-random cntx)
		  (mutator-context-variables cntx)))

(define (random-number cntx)
  (- (* 20.0 ((mutator-context-random cntx))) 10.0))

(define (random-operator cntx)
  (random-element (mutator-context-random cntx) 
		  (mutator-context-operators cntx)))

(define (times-1 n proc seq)
  (if (zero? n)
     seq
     (times-1 (- n 1) proc (cons (proc) seq))))

;; Generates a sequence n elements long of the result of proc.
(define (times n proc)
  (times-1 n proc '()))


;; Generates a random-expression.
(define (random-expression cntx depth)
  (case ((mutator-context-random cntx) 3)
  ((0) (random-variable cntx))
  ((1) (random-number cntx))
  ((2) (if (> depth 0)
	   (let ((ro (random-operator cntx)))
	     (set! depth (- depth 1))
	     (cons (car ro) 
		   (times (cadr ro) 
			  (lambda () (random-expression cntx depth))
			  )))
	   (case ((mutator-context-random cntx) 2)
	     ((0) (random-variable cntx))
	     ((1) (random-number cntx))
	   )))
    ))

;; Generates a random expression with expr as a subexpression.
(define (random-subexpression-with cntx expr)
  (let ((ro (random-operator cntx)))
    (cons
      (car ro)
      (random-list (mutator-context-random cntx)
		   (cons expr
			 (times (- (cadr ro) 1) (lambda () (random-expression cntx 1)))))
      )
    ))

(define (mutate-expr-2 cntx expr root)
  (cond 
    ((list? expr) 
     (case ((mutator-context-random cntx) 10)
       ;; generate a new random expression with expression
       ((0) (random-subexpression-with cntx expr))
       
       ;; Select a random subexpression from the root.
       ((1) (random-subexpression (mutator-context-random cntx) expr root))
       
       ;; randomize order of arguments
       ((2)
        (cons (car expr)
              (random-list (mutator-context-random cntx)
               (cdr expr))
              )
        )
       
       ;; generate a new expression using the same operator
       ((3)
        ;(display "(3) ")(write expr)(newline)
                
        (cons (car expr)
              (map (lambda (arg)
                     (mutate-expr-1 cntx arg root))
                   (cdr expr))
              )
        )
       ;;#|
       ;; generator a new expression using a different operator with same arguments.
       ((4)
        ;(display "(4) ") (write (car expr)) (newline)
        (cons (car (random-element 
		    (mutator-context-random cntx)
		    (mutator-context-operators-same-arity cntx (car expr))))
              (map (lambda (arg)
                     (mutate-expr-1 cntx arg root))
                     (cdr expr)
                   )
              )
        )
       ;;|#
       
       ;; select the expression.
       (else expr)
       )
     )
    
    ((symbol? expr)
     (case ((mutator-context-random cntx) 8)
       ;; replace with a new random expression.
       ((0) (random-expression cntx 1))
       ;; generate a new random expression with expression
       ((1) (random-subexpression-with cntx expr))
       ;; Select a random subexpression from the root.
       ((2) (random-subexpression (mutator-context-random cntx) expr root))
       ;; Select a random variable.
       ((3) (random-variable cntx))
       
       (else expr)
     ))
    
    (else 
     (case ((mutator-context-random cntx) 6)
       ;; replace with a new random expression.
       ((0) (random-expression cntx 1))
       ;; generate a new random expression with expression
       ((1) (random-subexpression-with cntx expr))
       ;; Select a random subexpression from the root.
       ((2) (random-subexpression (mutator-context-random cntx) expr root))
       
       (else expr)
       )
    )))

(define (mutate-expr-1 cntx expr root)
  ;(display "  ")(write `(mutate-expr-1 cntx ,expr root))(newline)
  
  (let ((result (mutate-expr-2 cntx expr expr)))
    ;(display "  ")(write `(mutate-expr-1 cntx ,expr root))(newline)(display "  => ")(write result)(newline)
    result))
;;(define mutate-expr-1 mutate-expr-2)

(define (mutate-expr cntx expr)
  ;(write `(mutate-expr cntx ,expr))(newline)
  
  (let ((result (mutate-expr-1 cntx expr expr)))
    ;(write `((mutate-expr cntx ,expr) => ,result))(newline)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (breed-exprs-1 cntx expr1 root1 expr2 root2)
  (cond
    ((and (list? expr1) (list? expr2) (= (length expr1) (length expr2)))
     (case ((mutator-context-random cntx) 6)
       ;; Select an operator from either side and arguments from either side
       ((0) 
        (cons (random-element (mutator-context-random cntx) (list (car expr1) (car expr2)))
              (random-element (mutator-context-random cntx) (list (cdr expr1) (car expr2))))
        )
       
       ;; Select an expression from either side and breed the arguments.
       ((1)
        (cons (random-element (mutator-context-random cntx) (list (car expr1) (car expr2)))
              (random-merge (mutator-context-random cntx) (cdr expr1) (car expr2)))
        )
       
       ;; Select an expression from either side of the same complexity.
       ((2)
        (let ((expr-root (random-element (mutator-context-random cntx) (list (cons (expr1 root2) (cons expr2 root1)))))
              )
          (random-subexpression (mutator-context-random cntx) (car expr-root) (cdr expr-root))
          )
        )
       
       (else (random-element (mutator-context-random cntx) (list expr1 expr2)))
       )
     )
    
    
    (else
     (random-element (mutator-context-random cntx) (list expr1 expr2))
     )
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cntx (mutator-context))
(define e1 (random-expression cntx 1))
(define e2 (random-expression cntx 1))


;(display (matrix-image m)) (newline)

(define (render-fxy fxy w xmin xmax h ymin ymax)
  (let ((m (matrix w h)))
    (matrix-fxy! m (lambda (x y)
                     ;(set! y (make-rectangular 0 y))
                     (fxy x y))
                 xmin xmax ymin ymax)
    (matrix-image-ppm m "mutate.pnm")
    ;; (matrix-image-plt m)
    ))

(define (v-func-namespace)
  (environment '(mutate v-func) '(rnrs))) ;; r6rs


(define (render-expr expr w xmin xmax h ymin ymax)
  (let* ((fexpr `(lambda (x y) (v-real-part ,(csee expr))))
	 (dummy1 (begin (display fexpr)(newline)))
	 (fxy (eval fexpr (v-func-namespace)))
	 )
    (render-fxy fxy w xmin xmax h ymin ymax)))

(define (mep)
  (do 
      ((e1-old e1))
    ((not (equal? e1 e1-old)) e1)
    (set! e1 (mutate-expr cntx e1))
    )
  ;(write e1) (newline)
  (list
     e1
     #|
     (plot (shade e1-lambda
                  ;#:levels 20
                  ;#:samples 75
                  ;#:title e1
                  
                  )
           ;#:x-min -10
           ;#:x-max 10
           ;#:y-min -10
           ;#:y-max 10
           )
     |#
     
     #|
     (plot3d (surface e1-lambda
                      )
             )
|#
     )
  )


(times 40 mep)
(write e1)(newline)
;(define e2 (csee e1))
;(write e2)(newline)
(render-expr e1 200 -10 10 200 -10 10)
;(render-expr e2 200 -10 10 200 -10 10)


