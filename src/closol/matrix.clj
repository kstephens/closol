(ns closol.matrix)

(define (matrix w h . default)
  (let* ((v (make-vector h)))
    (do ((j 0 (+ j 1)))
      ((= j h))
      (vector-set! v j (make-vector w (if (null? default) 0 (car default))))
      )
    (list->vector `(matrix width ,w height ,h data ,v)) 
    ))
 
(define (matrix-width m)
  (vector-ref m 2))
 
(define (matrix-height m)
  (vector-ref m 4))
 
(define (matrix-data m)
  (vector-ref m 6))
 
(define (matrix-ref m i j)
  (vector-ref (vector-ref (matrix-data m) j) i))
 
(define (matrix-set! m i j v)
  (vector-set! (vector-ref (matrix-data m) j) i v)
  m)
 
(define (matrix-do m fvij)
  (do ((j 0 (+ j 1)))
    ((= j (matrix-height m)))
    (do ((i 0 (+ i 1)))
      ((= i (matrix-width m)))
      (fvij (matrix-ref m i j) i j)))
  m)
 
(define (matrix-map! m fvij)
  (display '(matrix-map!))(newline)
  (matrix-do m (lambda (v i j)
                 (matrix-set! m i j (fvij v i j)))
    )
  m)
 
(define (matrix-map m fvij)
  (let ((m1 (matrix (matrix-width m) (matrix-height m))))
    (matrix-do m 
      (lambda (v i j)
        (matrix-set! m1 i j (fvij v i j))
        )
      )
    m1))

 
(define (matrix-fxy! m fxy xmin xmax ymin ymax)
  (matrix-do m 
    (lambda (v i j)
      (let* ((xf (v-lerp-1 i 0 (- (matrix-width m) 1)))
              (yf (v-lerp-1 j 0 (- (matrix-height m) 1)))
              (x (v-lerp xf xmin xmax))
              (y (v-lerp yf ymin ymax)))
        (matrix-set! m i j (fxy x y))
        )
      )
    )
  m)

(define (matrix-min-max m)
  (let* ((min (matrix-ref m 0 0))
	  (max min)
	  )
    (matrix-do m 
      (lambda (v i j)
        (if (> min v)
          (set! min v))
        (if (< max v)
          (set! max v))
        ))
    (list min max)))

 ;; Maps all elements:
 ;; [min, max] => [vmin, vmax].
(define (matrix-range! m vmin vmax)
  (let* ((mmin-max (matrix-min-max m))
	  (mmin (car mmin-max))
	  (mmax (cadr mmin-max))
	  (mscale (- mmax mmin)))
    (if (<= mscale 0)
      (set! mscale 1))
    
    (matrix-map! m 
      (lambda (v i j)
        (v-lerp (/ (- v mmin) mscale) vmin vmax)))
    ))

(define (matrix-data-vector m)
  (let ((rv (make-vector (* (matrix-width m) (matrix-height m))))
	 (vi 0))
    (matrix-do m (lambda (v i j)
                   (vector-set! rv vi v)
                   (set! vi (+ vi 1))))
    rv))
 
(define (matrix-make-graymap! m)
  (display '(matrix-make-graymap))(newline)
  (display `("  m => " ,m))(newline)
  (matrix-map! m (lambda (v i j)
                   (v-real-part v)))
  (display `("  v-real-part v => " ,m))(newline)
  (matrix-range! m 0 255)
  (display `("  matrix-range! => " ,m))(newline)
  (matrix-map! m (lambda (v i j)
                   (set! v (v-real-part v))
                   (exact (truncate v))))
  (display `("  map to integer => " ,m))(newline)
  m
  )

