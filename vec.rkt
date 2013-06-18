#lang racket

(require (planet wmfarr/plt-linalg:1:13/vector)
         (planet wmfarr/plt-linalg:1:13/matrix)
         "properties.rkt")

(provide approx
         *precision*
         set-precision!
         with-precision
         (struct-out vec)
         vec->list
         list->vec
         vec-length
         vec-angle
         vec+
         vec-
         vec*
         aligned?
         list->matrix
         translation-matrix
         rotation-matrix
         shear-matrix
         scale-matrix
         transform
         translate
         rotate
         scale
         skew-x
         skew-y
         reflect-x
         reflect-y
         vec-quick-scale)

;(struct vec (x y) #:transparent)
;
;(define (vec->complex v) (make-rectangular (vec-x v) (vec-y v)))
;
;(define (complex->vec c) (vec (real-part c) (imag-part c)))
;
;(define (add v1 . vs)
;  (complex->vec (apply + (map vec->complex (cons v1 vs)))))

(define *precision* 0.0000001)

(define (set-precision! p) (set! *precision* p))

(define-syntax-rule (with-precision (precision) body ...)
  (let ([old-precision *precision*])
    (begin (set-precision! precision)
           (let ([result body ...])
             (begin
               (set-precision! old-precision)
               result)))))
        

(define (approx n) (* (exact-round (/ n *precision*)) *precision*))
;
;(define (vec x y) (make-rectangular x y))
;(define (polar l a) (make-polar l a))
;
;(define (x v) (real-part v))
;(define (y v) (imag-part v))
;
;(define (len v) (magnitude v))
;
;(define (rotate v a) (* (exp (make-rectangular 0 a)) v))


(struct vec (x y) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (vec-transform v m)))

(define (list->vec l)
  (vec (car l) (cadr l)))

(define (vec->list v)
  (list (vec-x v) (vec-y v)))

(define (square n) (* n n))

(define (vec-length v) 
  (sqrt (+ (square (vec-x v))
           (square (vec-y v)))))

(define (vec-angle v)
  (match v
    [(vec 0 0) (error "zero length vector passed to vec-angle")]
    [(vec x 0) (if (> x 0) 0 pi)]
    [(vec 0 y) (* (/ pi 2) (if (> y 0) 1 3))]
    [_ (atan (/ (vec-y v) (vec-x v)))]))

(define (list->matrix lst)
  (if (null? lst) (error "Can't build a matrix from an empty list")
      (let ([r (length lst)]
            [c (length (car lst))])
        (apply matrix (append (list r c)
                              (flatten
                               (foldr (lambda (a b) (map cons a b))
                                      (make-list c null)
                                      lst)))))))
    
(define (vec-transform v m)
  (let* ([vm (matrix 3 1 (vec-x v) (vec-y v) 1)]
         [res (matrix-mul m vm)])
    (vec (approx (matrix-ref res 0 0)) (approx (matrix-ref res 1 0)))))


(define (vec+ v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

(define (vec- v1 v2)
  (vec+ v1 (vec* v2 -1)))

(define (vec* v n)
  (vec (* (vec-x v) n)
       (* (vec-y v) n)))

(define (aligned? v1 v2 v3)
  (let* ([va (vec- v2 v1)]
         [vb (vec- v3 v2)]
         [la (vec-length va)]
         [lb (vec-length vb)])
    (if (or (= 0 la) (= 0 lb))
        #t
        (equal? (vec* va (/ 1 la))
                (vec* vb (/ 1 lb))))))


(define (translation-matrix x y)
  (matrix 3 3 1 0 0 0 1 0 x y 1))

(define (rotation-matrix angle)
  (matrix 3 3 (cos angle) (sin angle) 0 
          (- (sin angle)) (cos angle) 0
          0 0 1))

(define (scale-matrix sx sy)
  (matrix 3 3 sx 0 0 0 sy 0 0 0 1))

(define (shear-matrix sx sy)
  (matrix 3 3 1 sy 0 sx 1 0 0 0 1))


(define (transform v mat)
  (when (transform? v)
    ((transform-ref v) v mat)))

(define (translate o v2)
  (transform o (translation-matrix (vec-x v2) (vec-y v2))))

(define (rotate o angle)
  (transform o (rotation-matrix angle)))

(define (scale o sx [sy sx])
  (transform o (scale-matrix sx sy)))

(define (skew-x o angle)
  (transform o (shear-matrix (- (approx (tan angle))) 0)))

(define (skew-y o angle)
  (transform o (shear-matrix 0 (approx (tan angle)))))

(define (reflect-x o)
  (transform o (scale-matrix -1 1)))
             
(define (reflect-y o)
  (vec-transform o (scale-matrix 1 -1)))

(define (vec-quick-scale v sx [sy sx])
  (vec (* (vec-x v) sx) 
       (* (vec-y v) sy)))


