#lang racket

(require "vec.rkt")

(provide (all-defined-out))

; Data Definition
; A Bezier is a list of vec, every vec represent the position of a node
; The number of nodes should be (+ 1 (* g s)), where s is the numer of segments
; and g is the order of the bezier

; a Segment is a Beziers with g + 1 nodes

; Example
; (list (vec 0 0) (vec 0 10) (vec 10 20) (vec 20 20))

; BoundingBox
; is represented by a pair of Vec, the first one is the lower left point,
; the second one is the upper right point
; Example
; (cons (vec 0 0) (vec 40 20))

; n-groups
; List [Any], Natural -> List of List [Any]
; (n-groups '(a b c d e f) 2) -> '((a b) (b c) (c d) (d e) (e f))
; (n-groups '(a b c d e f g) 3) -> '((a b c) (c d e) (e f g))

(define (n-groups lst n)
  (if (null? (cdr lst)) 
      null
      (let-values ([(f rest) (split-at lst (- n 1))])
        (cons (append f (list (car rest)))
              (n-groups rest n)))))


; closed? 
; Bezier -> Boolean
; check if the first and last node of the Bezier are equal

(define (closed? b)
  (equal? (car b) (last b)))

; segments
; Bezier, Natural -> list of Segments
; produce the list of Segments in which the gth order Bezier can be divided 

(define (segments b g)
  (if (> (remainder (- (length b) 1) g) 0)
      (error "Segments: Invalid number of points")
      (n-groups b (+ g 1))))



; on-curve-nodes
; Bezier , Natural-> list of Vec
; produce a list of nodes that lie on the Cubic Bezier (remove the 'control points')

(define (on-curve-nodes b g)
  (append (map car (segments b g)) (list (last b))))

      
; split
; Segment, Number [0, 1] -> (Segment or False) (Segment or False)
; Split a segment according to de Castlejau's rule at time t

(define (split s t)
  (cond [(= t 0) (values #f s)]
        [(= t 1) (values s #f)]
        [else
         (letrec ([between-nodes 
                   (lambda (ns)
                     (vec+ (vec* (cadr ns) t) (vec* (car ns) (- 1 t))))]
                  [aux (lambda (nodes acc)
                         (if (null? (cdr nodes))
                             (append acc (list nodes))
                             (aux (map between-nodes (n-groups nodes 2))
                                  (append acc (list nodes)))))])
           (let ([stages (aux s '())])
             (values
              (map car stages)
              (reverse (map last stages)))))]))

; point-at
; Segment, Number [0, 1] -> Vec
; produce a Vec representing the point on the Bezier Segment at 'time' t

(define (point-at s t)
  (cond [(= t 0) (car s)]
        [(= t 1) (last s)]
        [else (let-values ([(s1 s2) (split s t)])
                (car s2))]))
                              
; polygonize-segment 
; Segment, Natural -> List of Vec
; Produce a list of Vec representing the Bezier Segment as a polygon with n sides

(define (polygonize-segment s n)
  (map (lambda (t) (point-at s t))
       (map (lambda (r) (* r (/ 1.0 n))) (range 0 (+ 1 n)))))


; extremes
; Segment, Natural -> List of 4 Vec
; Find the extremes for the Bezier Segment (in a very inelegant way)
; result are ordered in this way (minx miny maxx maxy)

(define (extremes s n)
  (let ([points (polygonize-segment s n)])
    (list (car (sort points < #:key vec-x))
          (car (sort points < #:key vec-y))
          (car (sort points > #:key vec-x))
          (car (sort points > #:key vec-y)))))



; bounding-box
; Segment -> BoundigBox
; produces the BoundigBox of the Segment

(define (bounding-box s)
  (let* ([precision 200]
         [ex (extremes s 200)])
    (cons (vec (vec-x (first ex)) (vec-y (second ex)))
          (vec (vec-x (third ex)) (vec-y (fourth ex))))))

; combine-bounding-boxes 
; BoundingBox(es) -> BoundingBox
; produce the BoundingBox of BoundingBoxes

(define (combine-bounding-boxes bb . bbs)
  (foldl (lambda (bb1 bb2)
           (cons (vec (min (vec-x (car bb1)) (vec-x (car bb2)))
                      (min (vec-y (car bb1)) (vec-y (car bb2))))
                 (vec (max (vec-x (cdr bb1)) (vec-x (cdr bb2)))
                      (max (vec-y (cdr bb1)) (vec-y (cdr bb2))))))
         bb bbs))

; bezier-bounding-box
; Bezier, Natural -> BoundingBox
; produce the BoundingBox for the Bezier of order n

(define (bezier-bounding-box b n)
  (let ([ss (segments b n)])
    (apply combine-bounding-boxes (map bounding-box ss))))

                 
