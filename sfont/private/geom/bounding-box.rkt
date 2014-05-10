#lang racket

(require "vec.rkt")

(provide 
 (contract-out
  [bounding-box/c (-> any/c boolean?)]
  [line-bounding-box (-> (cons/c vec? vec?) bounding-box/c)]
  [inside-bounding-box? (-> vec? bounding-box/c boolean?)]
  [combine-bounding-boxes (->* (bounding-box/c) () #:rest (listof bounding-box/c) bounding-box/c)]
  [overlap-bounding-boxes? (-> bounding-box/c bounding-box/c boolean?)]
  [include-bounding-box? (-> bounding-box/c bounding-box/c boolean?)]
  [bounding-box-width (-> bounding-box/c number?)]
  [bounding-box-height (-> bounding-box/c number?)]
  [bounding-box-max-x (-> bounding-box/c (or/c #f number?))]
  [bounding-box-min-x (-> bounding-box/c (or/c #f number?))]
  [bounding-box-max-y (-> bounding-box/c (or/c #f number?))]
  [bounding-box-min-y (-> bounding-box/c (or/c #f number?))]))
  
; BoundingBox
; is represented by a pair of Vec, the first one is the lower left point,
; the second one is the upper right point
; Example
; (cons (vec 0 0) (vec 40 20))

(define bounding-box/c 
  (flat-named-contract 'bounding-box/c 
                       (lambda (bb)
                         (or (eq? bb #f)
                             (and (cons? bb)
                                     (vec? (car bb))
                                     (vec? (cdr bb))
                                     (>= (vec-x (cdr bb))
                                         (vec-x (car bb)))
                                     (>= (vec-y (cdr bb))
                                         (vec-y (car bb))))))))

; (cons Vec Vec) -> BoundingBox
; find the Bounding box of the line 
(define (line-bounding-box l)
  (match l
    [(cons (vec x1 y1) (vec x2 y2))
     (cons (vec (min x1 x2) (min y1 y2))
           (vec (max x1 x2) (max y1 y2)))]))

; Vec BoundingBox -> Boolean
; True if the point is inside the Bounding Box
(define (inside-bounding-box? v bb)
  (match v
    [(vec x y)
     (match bb
       [(cons (vec xmin ymin) (vec xmax ymax))
        (and (not (< x xmin))
             (not (< y ymin))
             (not (> x xmax))
             (not (> y ymax)))])]))

; BoundingBox ... -> BoundingBox
; produce the BoundingBox of BoundingBoxes
(define (combine-bounding-boxes bb . bbs)
  (foldl (lambda (bb1 bb2)
           (cond [(not bb1) bb2]
                 [(not bb2) bb1]
                 [else
                  (cons (vec (min (vec-x (car bb1)) (vec-x (car bb2)))
                             (min (vec-y (car bb1)) (vec-y (car bb2))))
                        (vec (max (vec-x (cdr bb1)) (vec-x (cdr bb2)))
                             (max (vec-y (cdr bb1)) (vec-y (cdr bb2)))))]))
         bb bbs))


; BoundingBox BoundingBox -> Boolean
; True if the bounding boxes overlap
(define (overlap-bounding-boxes? bb1 bb2)
  (match (cons bb1 bb2)
    [(cons #f _) #f]
    [(cons _ #f) #f]
    [(cons (cons (vec minx1 miny1) (vec maxx1 maxy1))
           (cons (vec minx2 miny2) (vec maxx2 maxy2)))
     (let ([t1 (and (>= minx1 minx2) (<= minx1 maxx2))]
           [t2 (and (>= maxx1 minx2) (<= maxx1 maxx2))]
           [t3 (and (>= miny1 miny2) (<= miny1 maxy2))]
           [t4 (and (>= maxy1 miny2) (<= maxy1 maxy2))])
       (or (include-bounding-box? bb1 bb2)
           (include-bounding-box? bb2 bb1)
           (and (or t1 t2) (>= maxy1 miny2) (<= miny1 maxy2))
           (and (or t3 t4) (>= maxx1 minx2) (<= minx1 maxx2))))]))

; BoundingBox BoundingBox -> Boolean
; True if the second bounding boxe is inside the first
(define (include-bounding-box? bb1 bb2)
  (if (or (not bb1) (not bb2))
      #f
      (andmap (lambda (v) 
                (inside-bounding-box? v bb1))
              (list (car bb2) (cdr bb2)))))

; BoundingBox -> Number
; Produce the width of the BoundingBox
(define (bounding-box-width bb)
  (if bb
      (abs (- (vec-x (cdr bb)) (vec-x (car bb))))
      0))

; BoundingBox -> Number
; Produce the height of the BoundingBox
(define (bounding-box-height bb)
  (if bb
      (abs (- (vec-y (cdr bb)) (vec-y (car bb))))
      0))

; BoundingBox -> Number or False
(define (bounding-box-max-x bb)
  (if bb (vec-x (cdr bb)) #f))

; BoundingBox -> Number or False
(define (bounding-box-min-x bb)
  (if bb (vec-x (car bb)) #f))

; BoundingBox -> Number or False
(define (bounding-box-max-y bb)
  (if bb (vec-y (cdr bb)) #f))

; BoundingBox -> Number or False
(define (bounding-box-min-y bb)
  (if bb (vec-y (car bb)) #f))

