#lang racket

(require (planet wmfarr/plt-linalg:1:13/vector)
         (planet wmfarr/plt-linalg:1:13/matrix)
         "properties.rkt"
         "utilities.rkt"
         racket/generic)

(provide 
 with-precision
 (contract-out
  [*precision* real?]
  [set-precision! (-> real? any/c)]
  [approx (-> real? real?)]
  [struct vec ((x real?)
               (y real?))]
  [struct trans-mat ((x real?) 
                     (xy real?) 
                     (yx real?)
                     (y real?)
                     (x-offset real?)
                     (y-offset real?))]
  [vec= (-> vec? vec? boolean?)]
  [vec-approx= (-> vec? vec? boolean?)]
  [list->vec (-> (list/c real? real?) vec?)]
  [vec->list (-> vec? (list/c real? real?))]
  [vec-length (-> vec? (and/c real? positive?))]
  [vec-angle (-> vec? real?)]
  [vec+ (-> vec? vec? vec?)]
  [vec- (-> vec? vec? vec?)]
  [vec* (-> vec? real? vec?)]
  [vec/ (-> vec? real? vec?)]
  [aligned? (-> vec? vec? vec? boolean?)]
  [translation-matrix (-> real? real? trans-mat?)]
  [rotation-matrix (-> real? trans-mat?)]
  [scale-matrix (->* (real?) (real?) trans-mat?)]
  [shear-matrix (-> real? real? trans-mat?)]
  [trans-mat* (-> trans-mat? trans-mat? trans-mat?)]
  [dot-prod (-> vec? vec? real?)]
  [cross-prod-2d (-> vec? vec? real?)]
  [segment-intersection (-> vec? vec? vec? vec? (or/c vec? #f))]
  [signed-area (-> vec? vec? real?)]
  [signed-polygonal-area (-> (listof vec?) real?)]
  [intersect-hor (-> real? vec? vec? (or/c vec? #f))]
  [intersect-vert (-> real? vec? vec? (or/c vec? #f))]
  [pass-through-hor? (-> real? vec? vec? boolean?)]
  [pass-through-vert? (-> real? vec? vec? boolean?)]
  [transform (-> any/c trans-mat? any/c)]
  [translate (-> any/c real? real? any/c)]
  [scale (->* (any/c real?) (real?) any/c)]
  [rotate (-> any/c real? any/c)]
  [skew-x (-> any/c real? any/c)]
  [skew-y (-> any/c real? any/c)]
  [reflect-x (-> any/c any/c)]
  [reflect-y (-> any/c any/c)])
 )
  

;;; Generic interface for geometric transformations
(define-generics geometric
  (transform geometric m)
  (translate geometric x y)
  (scale geometric fx [fy])
  (rotate geometric a)
  (skew-x geometric a)
  (skew-y geometric a)
  (reflect-x geometric)
  (reflect-y geometric))

;;; DEFINITIONS 
;;; *precision* is a variable used to approximate numbers

(define *precision* 0.0001)

(define (set-precision! p) (set! *precision* p))

(define-syntax-rule (with-precision (precision) body ...)
  (let ([old-precision *precision*])
    (begin (set-precision! precision)
           (let ([result body ...])
             (begin
               (set-precision! old-precision)
               result)))))

; Real -> Real
; approximae number using the global variable *precision*

(define (approx n) 
  (if (integer? n) n
      (* (exact-round (/ n *precision*)) *precision*)))

;;; Vec
;;; (vec Real Real)
;;; examples
;;; (vec 10 0)
;;; (vec 10.1 2)
;;; (vec 12 1.2)
;;; (vec 1.23 3.29)


(struct vec (x y) 
  #:transparent
  #:guard (lambda (x y tn)
            (values (approx x)
                    (approx y)))
  #:methods gen:geometric
  [(define (transform v m)
     (let* ([vm (matrix 3 1 (vec-x v) (vec-y v) 1)]
            [res (matrix-mul (trans-mat->matrix m) vm)])
       (vec (matrix-ref res 0 0) (matrix-ref res 1 0))))
  (define (translate v x y)
    (vec (+ (vec-x v) x)
         (+ (vec-y v) y)))
  (define (scale v fx [fy fx])
    (vec (* (vec-x v) fx)
         (* (vec-y v) fy)))
  (define (rotate v a)
    (let ([c (cos a)]
          [s (sin a)]
          [x (vec-x v)]
          [y (vec-y v)])
      (vec (- (* c x) (* s y))
           (+ (* s x) (* c y)))))
  (define (skew-x v a)
    (vec (+ (vec-x v) (* (vec-y v) (- (approx (tan a)))))
         (vec-y v)))
  (define (skew-y v a)
    (vec (vec-x v)
         (+ (* (vec-x v) (approx (tan a))) (vec-y v))))
  (define (reflect-x v)
    (vec (- (vec-x v)) (vec-y v)))
  (define (reflect-y v)
    (vec (vec-x v) (- (vec-y v))))])


;;; TransformationMatrix
;;; (trans-mat Real Real Real Real Real Real)
(struct trans-mat (x xy yx y x-offset y-offset)
  #:transparent
  #:guard (lambda (x xy yx y x-offset y-offset tn)
            (values x
                    xy
                    yx
                    y
                    (approx x-offset)
                    (approx y-offset)))
  #:methods gen:geometric
  [(define (transform m1 m2)
     (trans-mat* m2 m1))
  (define (translate m x y)
    (struct-copy trans-mat m 
                 [x-offset (+ (trans-mat-x-offset m) x)]
                 [y-offset (+ (trans-mat-y-offset m) y)]))                 
  (define (scale m fx [fy fx])
    (match m
      [(trans-mat x xy yx y x-offset y-offset)
       (trans-mat (* fx x) (* fx xy) 
                  (* fy yx) (* fy y)
                  (* fx x-offset)
                  (* fy y-offset))]))
  (define (rotate m a)
    (transform m (rotation-matrix a)))
  (define (skew-x m a)
    (transform m (shear-matrix (- (approx (tan a))) 0)))
  (define (skew-y m a)
    (transform m (shear-matrix 0 (approx (tan a)))))
  (define (reflect-x m)
    (scale m -1 1))
  (define (reflect-y m)
    (scale m 1 -1))])


; Vec -> Boolean
; true if the coordinates are =
(define (vec= v1 v2)
  (and (= (vec-x v1) (vec-x v2))
       (= (vec-y v1) (vec-y v2))))

; Vec -> Boolean
; true if the coordinates are approx=
(define (vec-approx= v1 v2)
  (and (= (approx (vec-x v1))
          (approx (vec-x v2)))
       (= (approx (vec-y v1))
          (approx (vec-y v2)))))

; (list Real Real) -> Vec
; produce a vector from a two elements list
(define (list->vec l)
  (vec (car l) (cadr l)))

; Vec -> (list Real Real)
; produce a list of two numbers from a vector
(define (vec->list v)
  (list (vec-x v) (vec-y v)))

; Vec -> Real
; produce the vector's length
(define (vec-length v) 
  (sqrt (+ (square (vec-x v))
           (square (vec-y v)))))

; Vec -> Real
; produce the vector's angle (counterclockwise)
(define (vec-angle v)
  (match v
    [(vec 0 0) (error "zero length vector passed to vec-angle")]
    [(vec x 0) (if (> x 0) 0 pi)]
    [(vec 0 y) (* (/ pi 2) (if (> y 0) 1 3))]
    [_ (atan (/ (vec-y v) (vec-x v)))]))

; Vec Vec -> Vec
; produce a new vector summing the coord. of the vectors
(define (vec+ v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

; Vec Vec -> Vec
; produce a new vector subtracting the coord. of the vectors
(define (vec- v1 v2)
  (vec+ v1 (vec* v2 -1)))

; Vec Real -> Vec
; produce a new vector multiplying the coord. of the vector by the number
(define (vec* v n)
  (vec (* (vec-x v) n)
       (* (vec-y v) n)))

; Vec Real -> Vec
; produce a new vector dividing the coord. of the vector by the number
(define (vec/ v n)
  (vec (/ (vec-x v) n)
       (/ (vec-y v) n)))

; Vec Vec Vec -> Boolean
; true if the positions defined by the three vectors are aligned
(define (aligned? v1 v2 v3)
  (let* ([va (vec- v2 v1)]
         [vb (vec- v3 v2)]
         [la (vec-length va)]
         [lb (vec-length vb)])
    (if (or (= 0 la) (= 0 lb))
        #t
        (equal? (vec* va (/ 1 la))
                (vec* vb (/ 1 lb))))))



; Real Real -> TransformationMatrix
; produce a Translation Matrix
(define (translation-matrix x y)
  (trans-mat 1 0 0 1 x y))

; Real -> TransformationMatrix
; produce a Rotation Matrix
(define (rotation-matrix angle)
  (trans-mat (cos angle) (- (sin angle)) (sin angle) (cos angle) 0 0))

; Real [Real] -> TransformationMatrix
; produce a Scale Matrix
(define (scale-matrix fx [fy fx])
  (trans-mat fx 0 0 fy 0 0))

; Real Real -> TransformationMatrix
; produce a Shear Matrix
(define (shear-matrix fx fy)
  (trans-mat 1 fx fy 1 0 0))

; TransformationMatrix -> Matrix
; produce a matrix (imported from planet, see above) from the transformation matrix
(define (trans-mat->matrix m)
  (match m
    [(trans-mat x xy yx y x-offset y-offset)
     (matrix 3 3 x yx 0 xy y 0 x-offset y-offset 1)]))

; Matrix -> TransformationMatrix
; produce a transformation matrix from a matrix
(define (matrix->trans-mat m)
  (trans-mat (matrix-ref m 0 0) (matrix-ref m 0 1) (matrix-ref m 1 0) 
             (matrix-ref m 1 1) (matrix-ref m 0 2) (matrix-ref m 1 2)))

; TransformationMatrix TransformationMatrix -> TransformationMatrix
; multiply the transformation matrices
(define (trans-mat* m1 m2)
  (matrix->trans-mat 
   (matrix-mul (trans-mat->matrix m1)
               (trans-mat->matrix m2))))


; Vec Vec -> Real 
; dot product
(define (dot-prod v1 v2)
  (+ (* (vec-x v1) (vec-x v2))
     (* (vec-y v1) (vec-y v2))))

; Vec Vec -> Real
; 2D cross product
(define (cross-prod-2d v1 v2)
  (- (* (vec-x v1) (vec-y v2))
     (* (vec-y v1) (vec-x v2))))

; Vec Vec Vec Vec -> Vec or False
; intersection between PB and QD 
; see http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
(define (segment-intersection p b q d)
  (let* ([r (vec- b p)]
         [s (vec- d q)]
         [rxs (cross-prod-2d r s)])
    (if (= rxs 0)
        #f
        (let* ([t (/ (cross-prod-2d (vec- q p) s)
                     rxs)]
               [u (/ (cross-prod-2d (vec- q p) r)
                     rxs)])
          (if (or (< t 0) (> t 1)
                  (< u 0) (> u 1))
              #f
              (vec+ p (vec* r t)))))))


; Vec Vec -> Real
; produces the area of the triangle formed by two vectors using the cross product
; the area as a sign: when positive it means the triangle is 'counter-clockwise'
(define (signed-area v1 v2)
  (/ (cross-prod-2d v1 v2) 2))


; (listof Vec) -> Real
; produce the signed area (see above) of the polygon whose points are defined by the position vectors
; passed to the function
(define (signed-polygonal-area lv)
  (foldl + 0 (map (lambda (p) (apply signed-area p)) (n-groups lv 2))))


  
; Real Vec Vec -> Vec or False
; produce the intersection of the line defined by v1 and v2 with the horizontal line y=n
(define (intersect-hor n v1 v2)
  (cond [(= (vec-y v1) (vec-y v2) n) v1]
        [(= (vec-y v1) (vec-y v2)) #f]
        [(= (vec-x v1) (vec-x v2)) (vec (vec-x v1) n)]
        [else (let* ([v12 (vec- v1 v2)]
                     [nn (- n (vec-y v1))]
                     [pr (vec (approx (* nn (/ (vec-x v12) (vec-y v12)))) nn)])
                (vec+ pr v1))]))


; Real Vec Vec -> Vec or False
; produce the intersection of the line defined by v1 and v2 with the vertical line x=n
(define (intersect-vert n v1 v2)
  (cond  [(= (vec-x v1) (vec-x v2) n) v1]
         [(= (vec-x v1) (vec-x v2)) #f]
         [(= (vec-y v1) (vec-y v2)) (vec n (vec-y v1))]
         [else (let* ([v12 (vec- v1 v2)]
                      [nn (- n (vec-x v1))]
                      [pr (vec nn (approx (* nn (/ (vec-y v12) (vec-x v12)))))])
                 (vec+ pr v1))]))

; Real Vec Vec -> Boolean
; check if the horizontal line y=n pass through the segment v1-v2
(define (pass-through-hor? n v1 v2)
  (or (<= (vec-y v1) n (vec-y v2))
      (>= (vec-y v1) n (vec-y v2))))


; Real Vec Vec -> Boolean
; check if the vertical line x=n pass through the segment v1-v2
(define (pass-through-vert? n v1 v2)
  (or (<= (vec-x v1) n (vec-x v2))
      (>= (vec-x v1) n (vec-x v2))))