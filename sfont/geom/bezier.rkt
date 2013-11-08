#lang racket

(require racket/draw
         (prefix-in pict: slideshow/pict)
         "vec.rkt"
         "bounding-box.rkt"
         "../utilities.rkt")

(provide  
 (contract-out
  [bezier/c (-> any/c boolean?)]
  [segment/c (-> any/c boolean?)]
  [cubic-segment/c (-> any/c boolean?)]
  [cubic-bezier/c (-> any/c boolean?)]
  [closed-bezier/c (-> any/c boolean?)]
  [closed? (-> bezier/c boolean?)]
  [segments (->* (bezier/c) (natural-number/c) (listof bezier/c))]
  [on-curve-points (->* (bezier/c) (natural-number/c) (listof vec?))]
  [end-points (-> bezier/c (cons/c vec? vec?))]
  [off-curve-points (-> segment/c (listof vec?))]
  [line-segment? (-> segment/c boolean?)]
  [canonical-line-segment (-> cubic-segment/c cubic-segment/c)]
  [split (-> segment/c (real-in 0 1) (values segment/c segment/c))]
  [join-beziers (->* (bezier/c) () #:rest (listof bezier/c) bezier/c)]
  [point-at (-> segment/c (real-in 0 1) vec?)]
  [polygonize-segment (-> segment/c natural-number/c (listof vec?))]
  [end-points-bounding-box (-> segment/c bounding-box/c)]
  [end-points-at-extrema? (-> segment/c boolean?)]
  [segment-bounding-box (-> segment/c bounding-box/c)]
  [bezier-bounding-box (->* (bezier/c) (natural-number/c) bounding-box/c)]
  [bezier-signed-area (->* (bezier/c) (natural-number/c natural-number/c) real?)]
  [bezier-area (->* (bezier/c) (natural-number/c natural-number/c) (and/c real? positive?))]
  [clockwise? (-> bezier/c boolean?)]
  [clockwise (-> closed-bezier/c closed-bezier/c)]
  [cubic-bezier-intersections (-> cubic-bezier/c cubic-bezier/c (listof vec?))]
  [cubic-segment-intersections (-> cubic-segment/c cubic-segment/c (listof vec?))]
  [line-segment-intersections (-> segment/c segment/c (listof vec?))]
  [segment-intersect-hor (-> real? segment/c (listof vec?))]
  [segment-intersect-vert (-> real? segment/c (listof vec?))]
  [bezier-intersect-hor (->* (real? bezier/c) (natural-number/c) (listof vec?))]
  [bezier-intersect-vert (->* (real? bezier/c) (natural-number/c) (listof vec?))]
  [bezier-boundaries-hor (->* (real? bezier/c) (natural-number/c) bounding-box/c)]
  [point-inside-bezier? (-> vec? closed-bezier/c boolean?)]
  [bezier-subtract (-> closed-bezier/c closed-bezier/c (listof closed-bezier/c))]
  [bezier-union (-> closed-bezier/c closed-bezier/c (listof closed-bezier/c))]
  [bezier-intersection (-> closed-bezier/c closed-bezier/c (listof closed-bezier/c))]
  [split-at-point (-> segment/c vec? (values segment/c segment/c))]
  [bezier->path (-> cubic-bezier/c (is-a?/c dc-path%) (is-a?/c dc-path%))]
  [print-beziers (->* () () #:rest (listof cubic-bezier/c) pict:pict?)]))
  

; Data Definition
; A Bezier is a list of vec, every vec represent the position of a node
; The number of nodes should be (+ 1 (* g s)), where s is the numer of segments
; and g is the order of the bezier

; a Segment is a Beziers with g + 1 nodes

; Example
; (list (vec 0 0) (vec 0 10) (vec 10 20) (vec 20 20))




(define bezier/c (flat-named-contract 'bezier/c (listof vec?)))

(define segment/c (flat-named-contract 'segment/c (listof vec?)))

(define cubic-segment/c (flat-named-contract 'cubic-segment/c (list/c vec? vec? vec? vec?)))

(define cubic-bezier/c (flat-named-contract 'cubic-bezier/c 
                                            (and/c bezier/c 
                                                   (lambda (b) 
                                                     (= (remainder (- (length b) 1) 3) 0)))))

; Bezier -> Boolean
; check if the first and last node of the Bezier vec=
(define (closed? b)
  (vec= (car b) (last b)))

(define closed-bezier/c (flat-named-contract 'closed-bezier/c (and/c bezier/c closed?)))

; Bezier Natural -> list of Segments
; produce the list of Segments in which the gth order Bezier can be divided 
(define (segments b [g 3])
  (if (> (remainder (- (length b) 1) g) 0)
      (error "Segments: Invalid number of points")
      (n-groups b (+ g 1))))


; Bezier Natural-> list of Vec
; produce a list of nodes that lie on the Cubic Bezier (remove the 'control points')
(define (on-curve-points b [g 3])
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
  (andmap (lambda (i) (aligned? (car s) (cadr s) i))
          (cddr s)))

; CubicSegment -> CubicSegment 
; If it is a line of a cubic segment place the offcurve points at the endpoints
(define (canonical-line-segment s)
  (if (line-segment? s)
      (list (car s) (car s) (last s) (last s))
      s))

; Segment Real [0, 1] -> (Segment or Null) (Segment or Null)
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


; Bezier Bezier -> (listof Bezier)
; produce a list of Beziers subtracting the second curve from the first
(define (bezier-subtract b1 b2)
  (let ([bb1 (c-clockwise b1)]
        [bb2 (clockwise b2)])
    (let ([ints (cubic-bezier-intersections bb1 bb2)])
      (if (> (length ints) 1)
          (let* ([marks1 (mark-bezier-with-points bb1 ints)]
                [marks2 (mark-bezier-with-points bb2 ints)]
                [bc1 (remove-parts-inside marks1 bb2)]
                [bc2 (remove-parts-outside marks2 bb1)]
                [r (join-parts bc1 bc2)])
            (if (clockwise? b1)
                (map clockwise r)
                (map c-clockwise r)))
          (if (andmap (curryr point-inside-bezier? b1) (on-curve-points bb2))
              (if (clockwise? b1) 
                  (list b1 b2)
                  (list b1 bb2))
              (list b1))))))

; Bezier Bezier -> (listof Bezier)
; produce the union of two bezier curves
(define (bezier-union b1 b2)
  (let ([bb1 (c-clockwise b1)]
        [bb2 (c-clockwise b2)])
    (let ([ints (cubic-bezier-intersections bb1 bb2)])
      (if (> (length ints) 1)
          (let* ([marks1 (mark-bezier-with-points bb1 ints)]
                [marks2 (mark-bezier-with-points bb2 ints)]
                [bc1 (remove-parts-inside marks1 bb2)]
                [bc2 (remove-parts-inside marks2 bb1)]
                [r (join-parts bc1 bc2)])
            (if (clockwise? b1)
                (map clockwise r)
                (map c-clockwise r)))
          (cond [(andmap (curryr point-inside-bezier? b1) (on-curve-points bb2))
                 (list b1)]
                [(andmap (curryr point-inside-bezier? b2) (on-curve-points bb1))
                 (list b2)]
                [else (list b1 b2)])))))

; Bezier Bezier -> (listof Bezier)
; produce the intersection of two bezier curves
(define (bezier-intersection b1 b2)
  (let ([bb1 (c-clockwise b1)]
        [bb2 (c-clockwise b2)])
    (let ([ints (cubic-bezier-intersections bb1 bb2)])
      (if (> (length ints) 1)
          (let* ([marks1 (mark-bezier-with-points bb1 ints)]
                [marks2 (mark-bezier-with-points bb2 ints)]
                [bc1 (remove-parts-outside marks1 bb2)]
                [bc2 (remove-parts-outside marks2 bb1)]
                [r (join-parts bc1 bc2)])
            (if (clockwise? b1)
                (map clockwise r)
                (map c-clockwise r)))
          (cond [(andmap (curryr point-inside-bezier? b1) (on-curve-points bb2))
                 (list b2)]
                [(andmap (curryr point-inside-bezier? b2) (on-curve-points bb1))
                 (list b1)]
                [else null])))))

; MarkedSegments is a curve split in pieces with a vec marking the place of split

; MarkedSegments MarkedSegments -> (listof Bezier)
(define (join-parts m1 m2)
  (letrec ([aux 
            (lambda (a b acc)
              (match a
                [(list-rest (? vec? v) (? bezier/c bs) (? vec? v1) r)
                 (let ([next (cond [(null? acc) (list bs)]
                                   [(closed? (car acc))
                                    (cons bs acc)]
                                   [else (cons (append (car acc) (cdr bs))
                                               (cdr acc))])])
                  
                   (if (or (= (length b) 1) (closed? (car next)))
                       (aux b r next)
                       (aux (find-mark b v1) r next)))] 
                
                [(list-rest (? vec? v) (? vec? v1) r)
                 (if (closed? (car acc))
                     (aux r b acc)
                     (aux (find-mark b v) (cdr a) acc))]
                [(list (? vec? v)) 
                 (if (closed? (car acc))
                     acc
                     (aux (find-mark b v) (cdr a) acc))]
                [(list) acc]))])
    (aux (join-between-marks m1)
         (join-between-marks m2)
         null)))

; MarkedSegments -> MarkedSegments
(define (join-between-marks m)
  (reverse
   (foldl (lambda (m acc)
            (if (vec? m)
                (cons m acc)
                (cond [(null? acc)
                       m]
                      [(vec? (car acc))
                       (cons m acc)]
                      [else (cons (append (car acc) (cdr m)) (cdr acc))])))
          null
          (set-mark-first m))))

; MarkedSegments -> MarkedSegments
(define (set-mark-first m)
  (cond [(and (vec? (car m)) 
              (bezier/c (cadr m)))
         m]
        [(andmap vec? m) m]
        [else (set-mark-first (append (cdr m) (list (car m))))]))

; MarkedSegments Vec -> MarkedSegments
(define (find-mark m v)
  (if (and (vec? (car m))
           (vec= (car m) v))
      m
      (find-mark (append (cdr m) (list (car m))) v)))
    
; MarkedSegments Bezier -> MarkedSegments
(define (remove-parts-inside m b)
  (filter (lambda (s)
            (or (vec? s)
                (not (point-inside-bezier? (point-at s 0.5) b))))
          m))

; MarkedSegments Bezier -> MarkedSegments
(define (remove-parts-outside m b)
  (filter (lambda (s)
            (or (vec? s)
                (point-inside-bezier? (point-at s 0.5) b)))
          m))

; Bezier (listof Vec) -> MarkedSegments
(define (mark-bezier-with-points b ints)
  (let ([s (split-bezier-with-points b ints)])
    (foldl (lambda (seg acc)
             (if (memf (curry vec= (car seg)) ints)
                 (append acc (cons (car seg) (list seg)))
                 (append acc (list seg))))
           null
           s)))

; Bezier (listof Vec) -> (listof Segments)
(define (split-bezier-with-points b ints)
  (foldl (lambda (i acc)
           (append*
            (map (lambda (s)
                   (let-values ([(s1 s2) (split-at-point s i)])
                     
                     (if (or (null? s1) (null? s2))
                         (list s)
                         (list (append (drop-right s1 1)
                                       (list i))
                               (cons i (cdr s2))))))
                 acc)))
         (segments b)
         ints))


; Segment Vec -> (Segment or Null) (Segment or Null)
; split the segment at coordinates v
(define (split-at-point s v)
  (cond [(line-segment? s) (split-line-segment-at-point s v)]
        [else (split-curve-segment-at-point s v)]))
  

; LineSegment Vec -> (Segment or Null) (Segment or Null)
(define (split-line-segment-at-point s v)
  (let ([ep (end-points s)])
    (cond [(vec-approx= (car ep) v)
           (values null s)]
          [(vec-approx= (cdr ep) v)
           (values s null)]
          [else (let* ([d1 (vec- (cdr ep) (car ep))]
                       [d2 (vec- v (car ep))])
                  
                  (if (parameterize ([precision 1])
                        (and
                         (>= (vec-length d1) (vec-length d2))
                         (vec-approx= (vec* d1 
                                            (/ (vec-length d2)
                                               (vec-length d1))) 
                                      d2)))
                      (values (list (car ep) (car ep) v v)
                              (list v v (cdr ep) (cdr ep)))
                      (values null null)))])))
    
  


; Segment Vec -> (Segment or Null) (Segment or Null)
; Split a segment at coordinates v
(define (split-curve-segment-at-point s v)
  (letrec ([aux (lambda (s1 start end)
                  (cond [(< (vec-length (vec- (car s1) v)) 2) start]
                          [(< (vec-length (vec- (last s1) v)) 2) end]
                          [(not (inside-bounding-box? v (segment-bounding-box s1))) #f]
                          [else (let-values ([(a b) (split s1 0.5)])
                                  (let ([fa (aux a start (+ start (* 0.5 (- end start))))])
                                    (if fa fa
                                        (aux b (+ start (* 0.5 (- end start))) end))))]))])
    (let ([f (aux s 0 1)])
      (if f (split s f) (values null null)))))


; Bezier ... -> Bezier
; produce a bezier joining beziers, if endpoints are equal
(define (join-beziers s1 . ss)
  (cond [(null? ss) s1]
        [(vec-approx= (last s1) (car (car ss)))
         (apply join-beziers (append s1 (cdr (car ss))) (cdr ss))]
        [else (error "I can't join the curves")]))
          
         

; Segment Real [0, 1] -> Vec
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



; Segment -> BoundingBox
; find the bounding box of the line connecting the endpoints
(define (end-points-bounding-box s)
  (line-bounding-box (end-points s)))


; Segment -> Boolean
; True if end points are at the extrema
(define (end-points-at-extrema? s)
  (let ([bb (end-points-bounding-box s)])
    (andmap (lambda (v) (inside-bounding-box? v bb))
            (off-curve-points s))))


; REMOVE?

; Segment Natural -> (list Vec Vec Vec Vec)
; Find the extremes for the Bezier Segment (in a very inelegant way)
; result are ordered in this way (minx miny maxx maxy)
(define (extremes s [n 3])
  (let ([points (polygonize-segment s n)])
    (list (car (sort points < #:key vec-x))
          (car (sort points < #:key vec-y))
          (car (sort points > #:key vec-x))
          (car (sort points > #:key vec-y)))))


; Segment -> BoundigBox
; produces the BoundigBox of the Segment
(define (segment-bounding-box s)
  (if (end-points-at-extrema? s)
      (end-points-bounding-box s)
      (let-values ([(s1 s2) (split s 0.5)])
        (combine-bounding-boxes (segment-bounding-box s1)
                                (segment-bounding-box s2)))))

; Bezier Natural -> BoundingBox
; produce the BoundingBox for the Bezier of order n
(define (bezier-bounding-box b [n 3])
  (cond [(null? b) #f]
        [(= (length b) 1) (cons (car b) (car b))]
        [else 
         (let ([ss (segments b n)])
           (apply combine-bounding-boxes (map segment-bounding-box ss)))]))


; Bezier Natural Natural -> Real
; produce the 'signed' area of a bezier (positive if counter-clockwise) of nth order
; every segment of a bezier is reduced to a polygon of s sides
(define (bezier-signed-area b [n 3] [s 200])
  (foldl + 0
         (map (lambda (se) (signed-polygonal-area (polygonize-segment se s)))
              (segments b n))))


; Bezier Natural Natural -> PositiveReal
; produce the area of a bezier (positive if counter-clockwise) of nth order
; every segment of a bezier is reduced to a polygon of s sides
(define (bezier-area b [n 3] [s 200])
  (abs (bezier-signed-area b n s)))


; Bezier -> Boolean
; check if the orientation of a bezier of nth order is clockwise
; every segment of a bezier is reduced to a polygon of s sides
(define (clockwise? b)
  (< (signed-polygonal-area b) 0))

; ClosedBezier -> ClosedBezier
; produces a new clockwise bezier 
(define (clockwise b)
  (if (clockwise? b) b (reverse b)))

; ClosedBezier -> ClosedBezier
; produces a new counter-clockwise bezier 
(define (c-clockwise b)
  (if (not (clockwise? b)) b (reverse b)))

; Vec ClosedBezier -> Boolean
; true if the point represented by vec is inside the bezier (or on the boundary)
(define (point-inside-bezier? v b)
  (let ([ints (bezier-intersect-hor (vec-y v) b)])
    (if (null? ints)
        #f
        (if (or
             (odd? 
              (length
               (filter (lambda (i)
                         (>= (vec-x v) (vec-x i)))
                       ints)))
             (odd?
              (length 
               (filter (lambda (i)
                         (>= (vec-x i) (vec-x v)))
                       ints))))
            #t
            #f))))

; CubicBezier CubicBezier -> (listOf Vec)
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

; CubicSegment CubicSegment -> (listOf Vec)
; produce a list of intersections between two bezier segments
(define (cubic-segment-intersections s1 s2)
  
  (let ([bb1 (segment-bounding-box s1)]
        [bb2 (segment-bounding-box s2)]
        [ep1 (end-points s1)]
        [ep2 (end-points s2)])
    (if (not (overlap-bounding-boxes? bb1 bb2))
        '()
        (cond [(and (line-segment? s1)
                    (line-segment? s2))
               (let ([i (segment-intersection (car ep1) (cdr ep1)
                                              (car ep2) (cdr ep2))])
                 (if i i '()))]
              [(line-segment? s1)
               (line-segment-intersections s1 s2)]
              [(line-segment? s2)
               (line-segment-intersections s2 s1)]
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
               (cubic-bezier-intersections 
                (call-with-values (lambda () (split s1 0.5)) join-beziers)
                (call-with-values (lambda () (split s2 0.5)) join-beziers))]))))

    
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
               (< (- (+ (vec-length (vec- v (car l)))
                        (vec-length (vec- (last l) v)))
                     (vec-length (vec- (last l) (car l))))
                  0.01))
              (map (lambda (v) (translate (rotate v a) (vec-x f) (vec-y f)))
                   (segment-intersect-hor 0 ts)))))
    


; Real Segment -> (listOf Vec)
; produce the list of intesection between the Bezier segment and the horizontal line y=h
(define (segment-intersect-hor h s)
  (remove-duplicates
   (let* ([ep (end-points s)]
          [vd (vec- (cdr ep) (car ep))]
          [bb (segment-bounding-box s)])
       (cond [(or (< h (vec-y (car bb)))
                  (> h (vec-y (cdr bb))))
              null]
             [(= (vec-y (car ep)) h) (list (car ep))]
             [(= (vec-y (cdr ep)) h) (list (cdr ep))]
             [(and (end-points-at-extrema? s)
                   (< (vec-length vd) 0.002)
                   (pass-through-hor? h (car ep) (cdr ep)))
              (list (intersect-hor h (car ep) (cdr ep)))]
             [else (let-values ([(sa sb) (split s 0.5)])
                     (append (segment-intersect-hor h sa)
                             (segment-intersect-hor h sb)))]))))
  

; Number Segment -> (listOf Vec)
; produce the list of intesection between the Bezier segment and the vertical line x=v
(define (segment-intersect-vert v s)
  (remove-duplicates
   (let* ([ep (end-points s)]
          [vd (vec- (cdr ep) (car ep))]
          [bb (segment-bounding-box s)])
       (cond [(or (< v (vec-x (car bb)))
                  (> v (vec-x (cdr bb))))
              null]
             [(= (vec-x (car ep)) v) (list (car ep))]
             [(= (vec-x (cdr ep)) v) (list (cdr ep))]
             [(and (end-points-at-extrema? s)
                   (< (vec-length vd) 0.002)
                   (pass-through-vert? v (car ep) (cdr ep)))
              (list (intersect-vert v (car ep) (cdr ep)))]
             [else (let-values ([(sa sb) (split s 0.5)])
                     (append (segment-intersect-vert v sa)
                             (segment-intersect-vert v sb)))]))))
 

; Real Bezier Natural -> (listOf Vec)
; produce the list of intesection between the Bezier curve (of nth order) and the horizontal line y=h
(define (bezier-intersect-hor h b [n 3])
  (remove-duplicates
          (foldl (lambda (s acc)
                   (append acc (segment-intersect-hor h s)))
                 '()
                 (segments b n))
          vec=))


; Real Bezier Natural -> (listOf Vec)
; produce the list of intesection between the Bezier curve (of nth order) and the vertical line x=v
(define (bezier-intersect-vert v b [n 3])
  (remove-duplicates
          (foldl (lambda (s acc)
                   (append acc (segment-intersect-vert v s)))
                 '()
                 (segments b n))
          vec=))


; Number, Bezier, Natural -> BoundingBox
; produce the BoundingBox (of zero height) defined by the min and max intersection of the bezier curve 
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
