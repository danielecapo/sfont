#lang racket

(require racket/draw
         (prefix-in pict: slideshow/pict)
         "vec.rkt"
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



; Bezier -> Boolean
; check if the first and last node of the Bezier are equal
(define (closed? b)
  (equal? (car b) (last b)))


; Bezier Natural -> list of Segments
; produce the list of Segments in which the gth order Bezier can be divided 
(define (segments b [g 3])
  (if (> (remainder (- (length b) 1) g) 0)
      (error "Segments: Invalid number of points")
      (n-groups b (+ g 1))))


; Bezier Natural-> list of Vec
; produce a list of nodes that lie on the Cubic Bezier (remove the 'control points')
(define (on-curve-nodes b [g 3])
  (append (map car (segments b g)) (list (last b))))

; Bezier -> (cons Vec Vec)
; produce a pair with the end points of the curve or segment
(define (end-points c)
  (cons (car c) (last c)))

; Segment -> (listOf Vec)
; produce the list of off-curve points of the segment
(define (off-curve-points s)
  (drop-right (cdr s) 1))
     
; Segment -> Boolean
; True if the points are aligned
(define (line-segment? s)
  (with-precision (0.1)
                  (if (< (length s) 3) #t
                      (andmap (lambda (i) (aligned? (car s) (cadr s) i))
                              (cddr s)))))

; Segment -> Segment 
; If it is a line of a cubic segment place the offcurve points at the endpoints
(define (canonical-line-segment s)
  (if (and (= (length s) 4)
           (line-segment? s))
      (list (car s) (car s) (last s) (last s))
      s))

; Segment Number [0, 1] -> (Segment or Null) (Segment or Null)
; Split a segment according to de Castlejau's rule at time t
(define (split s t)
  (cond [(= t 0) (values null s)]
        [(= t 1) (values s null)]
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

; Segment Vec -> (Segment or Null) (Segment or Null)
; Split a segment at coordinates v
(define (split-at-point s v)
  (letrec ([aux (lambda (s1 start end)
;                  (print start)
;                  (print " ")
;                  (print end)
;                  (newline)
                  (cond [(not (inside-bounding-box? v (bounding-box s1))) #f]
                        [(vec-approx= (car s1) v) start]
                        [(vec-approx= (last s1) v) end]
                        [(= (approx start) (approx end)) start]
                        [else (let-values ([(a b) (split s1 0.5)])
                                (let ([fa (aux a start (+ start (* 0.5 (- end start))))])
                                  (if fa fa
                                      (aux b (+ start (* 0.5 (- end start))) end))))]))])
    (let ([f (aux s 0 1)])
      (if f (split s f) (values null null)))))

; Bezier Vec Vec -> (Bezier or Null) (Bezier or Null)
; produce the two half of a closed bezier divided in A and B
(define (split-bezier-between b v1 v2 [n 3])
  (letrec ([aux (lambda (ss v prev)
                  (if (null? ss) prev
                      (let-values ([(sa sb) (split-at-point (car ss) v)])
                        (if (and (null? sa) (null? sb))
                            (aux (cdr ss) v (append prev (list (car ss))))
                            (cons (filter (lambda (x) (not (null? x))) 
                                                 (append prev (list sa)))
                                         (filter (lambda (x) (not (null? x)))
                                                 (append (list sb) (cdr ss))))))))])
    (let* ([s1 (aux (segments b n) v1 '())]
           [s2 (aux (car s1) v2 '())]
           [s3 (aux (cdr s1) v2 '())])
      (if (equal? (car s1) s2)
          (values (apply join-beziers (append (cdr s3) s2))
                  (apply join-beziers (car s3)))
          (values (apply join-beziers (cdr s2))
                  (apply join-beziers (append s3 (car s2))))))))

; Bezier Bezier -> Bezier
; subtract the second shape from the first
(define (bezier-subtract b1 b2)
  (if (or (not (closed? b1))
          (not (closed? b2)))
      (error "I can only subtract closed beziers")
      (let* ([a (if (clockwise? b1) (reverse b1) b1)]
             [b (if (clockwise? b2) b2 (reverse b2))]
             [is (cubic-bezier-intersections a b)])
        (if (not (= (length is) 2))
            (error "I can subtract shapes that intersect in two points")
            (let-values ([(a1 a2) (apply split-bezier-between a is)]
                         [(b1 b2) (apply split-bezier-between b is)])
              (let* ([ak (filter (lambda (i) (not (include-bounding-box?
                                                  (bezier-bounding-box b)
                                                  (bezier-bounding-box i))))
                                 (list a1 a2))]
                     [bk (filter (lambda (i) (include-bounding-box?
                                              (bezier-bounding-box a)
                                              (bezier-bounding-box i)))
                                 (list b1 b2))]
                     [ak1 (if (and (> (length ak) 1)
                                   (= (length bk) 1))
                              (list (car (sort ak < #:key (lambda (i) 
                                                            (+
                                                             (vec-length (vec- (first i) (last (car bk))))
                                                             (vec-length (vec- (last i) (first (car bk)))))))))
                              ak)]
                     [bk1 (if (and (> (length bk) 1)
                                   (= (length ak) 1))
                              (list (car (sort bk < #:key (lambda (i)
                                                            (+
                                                             (vec-length (vec- (first i) (last (car ak))))
                                                             (vec-length (vec- (last i) (first (car ak)))))))))
                              bk)])
                (if (and (= (length bk1) 1)
                         (= (length ak1) 1))
                    (apply join-beziers
                           (map canonical-line-segment
                                (segments (append (drop-right (car ak1) 1)
                                                  (drop-right (car bk1) 1)
                                                  (list (car (car ak1)))))))
                    (error "bezier subtraction: something went wrong"))))))))

; Bezier ... -> Bezier
; produce a bezier joining beziers, if endpoints are equal
(define (join-beziers s1 . ss)
  (cond [(null? ss) s1]
        [(vec-approx= (last s1) (car (car ss)))
         (apply join-beziers (append s1 (cdr (car ss))) (cdr ss))]
        [else (error "I can't join the curves")]))
          
         

; Segment Number [0, 1] -> Vec
; produce a Vec representing the point on the Bezier Segment at 'time' t
(define (point-at s t)
  (cond [(= t 0) (car s)]
        [(= t 1) (last s)]
        [else (let-values ([(s1 s2) (split s t)])
                (car s2))]))
                              
 
; Segment Natural -> List of Vec
; Produce a list of Vec representing the Bezier Segment as a polygon with n sides
(define (polygonize-segment s n)
  (map (lambda (t) (point-at s t))
       (map (lambda (r) (* r (/ 1.0 n))) (range 0 (+ 1 n)))))

; (cons Vec Vec) -> BoundingBox
; find the Bounding box of the line 
(define (line-bounding-box l)
  (match l
    [(cons (vec x1 y1) (vec x2 y2))
     (cons (vec (min x1 x2) (min y1 y2))
           (vec (max x1 x2) (max y1 y2)))]))

; Segment -> BoundingBox
; find the bounding box of the line connecting the endpoints
(define (end-points-bounding-box s)
  (line-bounding-box (end-points s)))

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

; Segment -> Boolean
; True if end points are at the extrema
(define (end-points-at-extrema? s)
  (let ([bb (end-points-bounding-box s)])
    (andmap (lambda (v) (inside-bounding-box? v bb))
            (off-curve-points s))))


; Segment Natural -> (list Vec Vec Vec Vec)
; Find the extremes for the Bezier Segment (in a very inelegant way)
; result are ordered in this way (minx miny maxx maxy)
(define (extremes s n)
  (let ([points (polygonize-segment s n)])
    (list (car (sort points < #:key vec-x))
          (car (sort points < #:key vec-y))
          (car (sort points > #:key vec-x))
          (car (sort points > #:key vec-y)))))


; Segment -> BoundigBox
; produces the BoundigBox of the Segment
(define (bounding-box s)
  (if (end-points-at-extrema? s)
      (end-points-bounding-box s)
      (let-values ([(s1 s2) (split s 0.5)])
        (combine-bounding-boxes (bounding-box s1)
                                (bounding-box s2)))))

; BoundingBox ... -> BoundingBox
; produce the BoundingBox of BoundingBoxes
(define (combine-bounding-boxes bb . bbs)
  (foldl (lambda (bb1 bb2)
           (cons (vec (min (vec-x (car bb1)) (vec-x (car bb2)))
                      (min (vec-y (car bb1)) (vec-y (car bb2))))
                 (vec (max (vec-x (cdr bb1)) (vec-x (cdr bb2)))
                      (max (vec-y (cdr bb1)) (vec-y (cdr bb2))))))
         bb bbs))

; BoundingBox BoundingBox -> Boolean
; True if the bounding boxes overlap
(define (overlap-bounding-boxes? bb1 bb2)
  (match (cons bb1 bb2)
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
  (andmap (lambda (v) 
            (inside-bounding-box? v bb1))
          (list (car bb2) (cdr bb2))))

; Bezier Natural -> BoundingBox
; produce the BoundingBox for the Bezier of order n
(define (bezier-bounding-box b [n 3])
  (let ([ss (segments b n)])
    (apply combine-bounding-boxes (map bounding-box ss))))


; Bezier Natural Natural -> Number
; produce the 'signed' area of a bezier (positive if counter-clockwise) of nth order
; every segment of a bezier is reduced to a polygon of s sides
(define (bezier-signed-area b [n 3] [s 200])
  (foldl + 0
         (map (lambda (se) (signed-polygonal-area (polygonize-segment se s)))
              (segments b n))))


; Bezier Natural Natural -> Number
; produce the area of a bezier (positive if counter-clockwise) of nth order
; every segment of a bezier is reduced to a polygon of s sides
(define (bezier-area b [n 3] [s 200])
  (abs (bezier-signed-area b n s)))


; Bezier Natural Natural -> Number
; check if the orientation of a bezier of nth order is clockwise
; every segment of a bezier is reduced to a polygon of s sides
(define (clockwise? b)
  (< (signed-polygonal-area b) 0))

; Bezier Bezier -> (listOf Vec)
(define (cubic-bezier-intersections b1 b2)
  (let* ([ss1 (segments b1 3)]
         [ss2 (segments b2 3)]
         [cs (apply append
                    (map (lambda (a)
                           (map (lambda (b) (list a b))
                                ss2))
                         ss1))])
    (remove-duplicates 
     (flatten (map (lambda (c) (apply cubic-segment-intersections c))
                  cs)))))

; Segment Segment -> (listOf Vec)
; produce a list of intersections between two bezier segments
(define (cubic-segment-intersections s1 s2)
  (let ([bb1 (bounding-box s1)]
        [bb2 (bounding-box s2)]
        [ep1 (end-points s1)]
        [ep2 (end-points s2)])
    (if (not (overlap-bounding-boxes? bb1 bb2))
        '()
;        (begin (print s1)
;               (newline)
;               (print s2)
;               (newline)
;               (print "....")
;               (newline)
        (cond [(and (line-segment? s1)
                    (line-segment? s2))
               (let ([i (segment-intersection (car ep1) (cdr ep1)
                                              (car ep2) (cdr ep2))])
                 (if i i '()))]
              [(line-segment? s1)
               (line-segment-intersections s1 s2)]
              [(line-segment? s2)
               (line-segment-intersections s2 s1)]
             ;
              [(and (end-points-at-extrema? s1)
                    (< (vec-length (vec- (car ep1) (cdr ep1)))
                       0.002))
               
               (let ([i (segment-intersection (car ep1) (cdr ep1)
                                              (car ep2) (cdr ep2))])
                     (if i i '()))]
              [(and (end-points-at-extrema? s2)
                    (< (vec-length (vec- (car ep2) (cdr ep2)))
                  0.002))
               
               (let ([i (segment-intersection (car ep1) (cdr ep1)
                                              (car ep2) (cdr ep2))])
                     (if i i '()))]
              [else 
;               (print ep1)
;               (newline)
;               (print ep2)
;               (newline)
               
               (cubic-bezier-intersections 
                       (call-with-values (lambda () (split s1 0.5)) join-beziers)
                       (call-with-values (lambda () (split s2 0.5)) join-beziers))]))))

;        (append* (if (or (vec-approx= (car ep1) (car ep2))
;                         (vec-approx= (car ep1) (cdr ep2)))
;                     (list (car ep1))
;                     '())
;                 (if (or (vec-approx= (cdr ep1) (car ep2))
;                         (vec-approx= (cdr ep1) (cdr ep2)))
;                     (list (cdr ep1))
;                     '())
;                 (let-values ([(sa1 sb1) (split s1 0.5)]
;                              [(sa2 sb2) (split s2 0.5)])
;                     
;                   (cubic-bezier-intersections (append sa1 (cdr sb1))
;                                               (append sa2 (cdr sb2))))))))
                  
    
; Segment Segment -> (listOf Vec)
; produce a list of intersections between the straight segment and the curved segment
(define (line-segment-intersections l s)
  (let* ([f (car l)]
         [a (vec-angle (vec- (car l) (last l)))]
         [ts (map (lambda (v) 
                    (rotate (translate v (- (vec-x f)) (- (vec-y f))) 
                            (- a)))
                  s)])
    (filter (lambda (v)
              (and (>= (vec-x v) (vec-x (car (bounding-box l))))
                   (<= (vec-x v) (vec-x (cdr (bounding-box l))))
                   (>= (vec-y v) (vec-y (car (bounding-box l))))
                   (<= (vec-y v) (vec-y (cdr (bounding-box l))))))
               (map (lambda (v) (translate (rotate v a) (vec-x f) (vec-y f)))
                    (segment-intersect-hor 0 ts)))))
    


; Number Segment -> (listOf Vec)
; produce the list of intesection between the Bezier segment and the horizontal line y=h
(define (segment-intersect-hor h s)
  (remove-duplicates
   (let ([bb (bounding-box s)])
     (if (or (< h (vec-y (car bb)))
             (> h (vec-y (cdr bb))))
         '()
         (let* ([ep (end-points s)]
                [v (vec- (cdr ep) (car ep))])
           (if (and (end-points-at-extrema? s)
                    (< (vec-length v) 0.002))
               (list (intersect-hor h (car ep) (cdr ep)))
               (let-values ([(sa sb) (split s 0.5)])
                 (append (segment-intersect-hor h sa)
                         (segment-intersect-hor h sb)))))))))



; Number Segment -> (listOf Vec)
; produce the list of intesection between the Bezier segment and the vertical line x=v
(define (segment-intersect-vert v s)
  (remove-duplicates
   (let ([bb (bounding-box s)])
     (if (or (< v (vec-x (car bb)))
             (> v (vec-x (cdr bb))))
         '()
         (let* ([ep (end-points s)]
                [vd (vec- (cdr ep) (car ep))])
           (if (and (end-points-at-extrema? s)
                    (< (vec-length vd) 0.002))
               (list (intersect-vert v (car ep) (cdr ep)))
               (let-values ([(sa sb) (split s 0.5)])
                 (append (segment-intersect-vert v sa)
                         (segment-intersect-vert v sb)))))))))



; Number Bezier Natural -> (listOf Vec)
; produce the list of intesection between the Bezier curve (of nth order) and the horizontal line y=h
(define (bezier-intersect-hor h b [n 3])
  (remove-duplicates
          (foldl (lambda (s acc)
                   (append acc (segment-intersect-hor h s)))
                 '()
                 (segments b n))
          vec=))


; Number Bezier Natural -> (listOf Vec)
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


; bezier->path
; Bezier, DrawingPath -> DrawingPath
; Draw the Cubic Bezier curve in the Drawing Path

(define (bezier->path b p)
  (let ([ss (segments b)]
        [f (car b)])
    (letrec ([aux (lambda (segments)
                    (match segments
                      [(list) (begin
                                (send p close)
                                p)]
                      [(list-rest (list (vec x y) (vec cx cy) (vec cx1 cy1) (vec x1 y1))
                                  r)
                       (begin 
                         (send p curve-to cx cy cx1 cy1 x1 y1)
                         (aux r))]))])
      (begin
        (send p move-to (vec-x f) (vec-y f))
        (aux ss)))))

; print-beziers
; Beziers Curves -> side effect
; print the beziers

(define (print-beziers . bs)
  (let* ([scene-side 500]
         [real-side 2000]
         [max-x 1000]
         [max-y 1000]
         [min-x (- max-x real-side)]
         [min-y (- max-y real-side)]
         [f (/ scene-side (- max-y min-y))]
         [path (new dc-path%)])
    (pict:dc
     (lambda (dc dx dy)
       (begin
         (send dc scale f (- f))
         (send dc translate (- min-x) (- max-y))
         (send dc set-pen "Gainsboro" 10 'solid)
         (send dc draw-line min-x 0 max-x 0)
         (send dc draw-line 0 max-y 0 min-y)
         (send dc set-brush "black" 'solid)
         (send dc set-pen "LightGray" 1 'solid)
         
         (for-each (lambda (b) (bezier->path b path)) bs)
         (send dc draw-path path dx dy 'winding)))
     scene-side scene-side)))




                 
