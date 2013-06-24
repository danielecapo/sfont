#lang racket

(require "vec.rkt"
         "utilities.rkt")

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

(define (bezier-bounding-box b [n 3])
  (let ([ss (segments b n)])
    (apply combine-bounding-boxes (map bounding-box ss))))

; bezier-signed-area
; Bezier, Natural, Natural -> Number
; produce the 'signed' area of a bezier (positive if counter-clockwise) of nth order
; every segment of a bezier is reduced to a polygon of s sides

(define (bezier-signed-area b [n 3] [s 200])
  (foldl + 0
         (map (lambda (se) (signed-polygonal-area (polygonize-segment se s)))
              (segments b n))))

; bezier-area
; Bezier,  Natural, Natural -> Number
; produce the area of a bezier (positive if counter-clockwise) of nth order
; every segment of a bezier is reduced to a polygon of s sides

(define (bezier-area b [n 3] [s 200])
  (abs (bezier-signed-area b n s)))

; clockwise
; Bezier,  Natural, Natural -> Number
; check if the orientation of a bezier of nth order is clockwise
; every segment of a bezier is reduced to a polygon of s sides

(define (clockwise? b [n 3] [s 200])
  (< (bezier-signed-area b n s) 0))

; segment-intersect-hor
; Number, Segment -> List of Vec
; produce the list of intesection between the Bezier segment and the horizontal line y=h

(define (segment-intersect-hor h s)
  (foldl (lambda (ls acc)
           (if (apply pass-through-hor? h ls)
               (cons (apply intersect-hor h ls) acc)
               (reverse acc)))
         '()
         (n-groups (polygonize-segment s 200) 2)))

; segment-intersect-vert
; Number, Segment -> List of Vec
; produce the list of intesection between the Bezier segment and the vertical line x=v

(define (segment-intersect-vert v s)
  (foldl (lambda (ls acc)
           (if (apply pass-through-vert? v ls)
               (cons (apply intersect-vert v ls) acc)
               (reverse acc)))
         '()
         (n-groups (polygonize-segment s 200) 2)))



; bezier-intersect-hor
; Number, Bezier, Natural -> List of Vec
; produce the list of intesection between the Bezier curve (of nth order) and the horizontal line y=h

(define (bezier-intersect-hor h b [n 3])
  (remove-duplicates
          (foldl (lambda (s acc)
                   (append acc (segment-intersect-hor h s)))
                 '()
                 (segments b n))
          vec=))

; bezier-intersect-vert
; Number, Bezier, Natural -> List of Vec
; produce the list of intesection between the Bezier curve (of nth order) and the vertical line x=v

(define (bezier-intersect-vert v b [n 3])
  (remove-duplicates
          (foldl (lambda (s acc)
                   (append acc (segment-intersect-vert v s)))
                 '()
                 (segments b n))
          vec=))


; bezier-boundaries-hor
; Number, Bezier, Natural -> BoundingBox
; produce the BoundingBox (of zero height) define by the min and max intersection of the bezier curve 
; with horizontal line y = h


(define (bezier-boundaries-hor h b [n 3])
  (let ([sorted-intersections 
         (sort (bezier-intersect-hor h b n)
               <
               #:key vec-x)])
    (cons (car sorted-intersections)
          (last sorted-intersections))))


    




                 
