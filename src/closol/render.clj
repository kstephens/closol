(ns closol.render)
;; NOT FINISHED PORTING TO CLOJURE.

;(display (matrix-image m)) (newline)

(defn render-fxy [fxy w xmin xmax h ymin ymax]
  (let [m (matrix w h)]
    (matrix-fxy! m #(fxy %1 %2)
      xmin xmax ymin ymax)
    (matrix-image-ppm m "mutate.pnm")
    ;; (matrix-image-plt m)
    ))

(defn v-func-namespace []
  (environment '(mutate v-func) '(rnrs))) ;; r6rs

(defn render-expr [expr w xmin xmax h ymin ymax]
  (let* ((fexpr `(lambda (x y) (v-real-part ,(csee expr))))
	 (dummy1 (begin (display fexpr)(newline)))
	 (fxy (eval fexpr (v-func-namespace)))
	 )
    (render-fxy fxy w xmin xmax h ymin ymax)))

(defn mep []
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


