#lang racket
(require "../../geometry.rkt"
         "../../utilities.rkt"
         (only-in "fontwriter.rkt" 
                  from
                  translate.
                  rotate.
                  scale.
                  skew-x.
                  skew-y.
                  reflect-x.
                  reflect-y.)
         (for-syntax racket/list
                     syntax/parse))

(provide 
 (contract-out
  [rect (-> real? real? real? real? cubic-bezier/c)]
  [ellipse (-> real? real? real? real? cubic-bezier/c)]
  [arc (-> real? real? real? real? cubic-bezier/c)]
  [remove~ (->* (and/c closed-bezier/c cubic-bezier/c) () #:rest (listof (and/c closed-bezier/c cubic-bezier/c)) 
                (listof (and/c closed-bezier/c cubic-bezier/c)))]
  [join~ (->* (and/c closed-bezier/c cubic-bezier/c) () #:rest (listof (and/c closed-bezier/c cubic-bezier/c)) 
              (listof (and/c closed-bezier/c cubic-bezier/c)))])
 ~)

; Vec Real Vec Real -> (Vec or False)
(define (line-intersection a alpha b beta)
  (let* ([det (- (* (sin alpha) (cos beta))
                 (* (cos alpha) (sin beta)))]
         [ba (vec- b a)]
         [bax (vec-x ba)]
         [bay (vec-y ba)])
    (if (= 0 det) 
        #f
        (let ([l (/ (- (* (cos beta) bay)
                       (* (sin beta) bax)) 
                    det)])
          (vec (+ (vec-x a) (* l (cos alpha)))
               (+ (vec-y a) (* l (sin alpha))))))))
          


(define-syntax (parse-curves stx)
  (syntax-parse stx 
    #:datum-literals (@ @° insert ><)
    [(_ (insert i:expr) path-element:expr . r)
     (syntax-parse #'path-element 
       #:datum-literals (insert @)
       [(@ insert o:expr) 
        #'(let* ([b i]
                 [n (car b)]
                 [l (last b)])
            (join-subpaths b (parse-curves (insert (translate. o (vec-x l) (vec-y l))) . r)))]
       [(@ x:expr y:expr)
        #'(let* ([b i]
                 [n (car b)]
                 [l (last b)]
                 [p (vec+ l (vec x y))])
            (join-subpaths b (parse-curves ((vec-x p) (vec-y p)) . r)))]
       [path-element 
        #'(let* ([b i]
                 [n (car b)])
            (join-subpaths b (parse-curves path-element . r)))])]
    [(_ (x1:expr y1:expr . t1) (>< a1:expr a2:expr . t2) (x2:expr y2:expr . t3) . r)
     #'(let* ([xp x1]
              [yp y1]
              [xn x2]
              [yn y2]
              [angle1 a1]
              [angle2 a2]
              [i (line-intersection (vec xp yp) angle1
                                    (vec xn yn) angle2)])
         (if (not i)
             (error "Angles don't converge.")
             (parse-curves (xp yp . t1) ((vec-x i) (vec-y i) . t2) (xn yn . t3) . r)))]
    [(_ (x:expr y:expr) path-element:expr . r)
     (syntax-parse #'path-element 
       #:datum-literals (@ @° insert)
       [(insert i:expr) #'(join-subpaths (list (vec x y)) (parse-curves  (insert i) . r))]
       [(@ insert o:expr) 
        #'(let ([n (vec x y)])
            (join-subpaths 
             (list n) 
             (parse-curves (insert (translate. o (vec-x n) (vec-y n))) . r)))]
       [(@ cx:expr cy:expr . t) 
        #'(let* ([n (vec x y)]
                 [nc (vec+ n (vec cx cy))])
            (parse-curves ((vec-x n) (vec-y n))
                          ((vec-x nc) (vec-y nc) . t)
                          . r))]
       [(@° a:expr l:expr . t) 
        #'(let ([n (vec x y)]
                [angle a]
                [len l])
            (parse-curves (x y) 
                          (@ (* len (cos angle))
                             (* len (sin angle))
                             . t)
                          . r))]
       [(cx:expr cy:expr t:expr) 
        #'(let ([tension t])
            (parse-curves (x y) (cx cy tension tension) . r))]
       [(cx:expr cy:expr t:expr t1:expr) 
        #'(let ([n (vec x y)]
                [nt (vec cx cy)]
                [tension t1])
            (append (list n (vec+ n (vec* (vec- nt n) t)))
                    (parse-curves ((vec-x nt) (vec-y nt) tension) . r)))]
       [(x1:expr y1:expr)
        #'(cons (vec x y) (parse-curves (x1 y1) . r))])]
    [(_ (cx:expr cy:expr t:expr) path-element:expr . r)
     (syntax-parse #'path-element 
       #:datum-literals (@ @°)
       [(@ x1:expr y1:expr)
        #'(let* ([nt (vec cx cy)]
                 [n (vec+ nt (vec x1 y1))])
            (parse-curves ((vec-x nt) (vec-y nt) t)
                          ((vec-x n) (vec-y n)) . r))]
       [(@° a:expr l:expr)
        #'(let ([nt (vec cx cy)]
                [angle a]
                [len l])
            (parse-curves (cx cy t) 
                          (@ (* len (cos angle))
                             (* len (sin angle)))
                          . r))]
       [(x1:expr y1:expr)
        #'(let ([n (vec x1 y1)]
                [nt (vec cx cy)])
            (cons (vec+ n (vec* (vec- nt n) t)) 
                  (parse-curves ((vec-x n) (vec-y n)) . r)))]
       [path-element:expr 
        (raise-syntax-error #f "Invalid path element after tension point" stx #'path-element)])]
    [(_ (insert i:expr)) #'i]
    [(_ (x:expr y:expr)) #'(list (vec x y))]
    [(_ (x:expr y:expr . r))
     (raise-syntax-error #f "Invalid end path element" stx)]
    [(_) #'()]))




(define-syntax (~ stx)
  (syntax-parse stx 
    #:datum-literals (insert cycle --)
    [(_ f:expr c:expr ... cycle)
     (syntax-parse #'f 
       #:datum-literals (insert)
       [(insert i:expr) 
        #'(let* ([b i]
                 [n (car b)])
            (~ (insert b) c ... ((vec-x n) (vec-y n))))]
       [(x:expr y:expr) #'(let ([n (vec x y)])
                  (~ ((vec-x n) (vec-y n)) c ... ((vec-x n) (vec-y n))))]
       [x:expr (raise-syntax-error #f "Expected coordinates or (insert ...)" stx #'x)])]
    [(_ . r)
     (letrec ([p-lines (lambda (ps acc)
                         (syntax-parse ps 
                           #:datum-literals (-- insert)
                           [() acc]
                           
                           [(-- l . r)
                            (p-lines #'r (datum->syntax 
                                          stx 
                                          (append (syntax->list acc) 
                                                  (list #'(@ 0 0) #'l #'(@ 0 0)))))]
                           
                           [(f . r) 
                            (p-lines #'r (datum->syntax stx (append (syntax->list acc) (list #'f))))]))])
       (with-syntax ([(path-elts ...) (p-lines #'r #'())])
         #'(parse-curves path-elts ...)))]))



; Bezier, Bezier -> Bezier
; joins two subpaths with a straight line
(define (join-subpaths p1 p2)
  (cond [(null? p2) p1]
        [(null? p1) p2]
        [(null? (cdr p1))
         (let ([p (car p1)])
           (if (vec= p (car p2))
               (cons p (cdr p2))
               (append (list p p (car p2)) p2)))]
        [else (cons (car p1) (join-subpaths (cdr p1) p2))]))



; Bezier  Bezier ... -> (listof Bezier)
(define (remove~ pt . from-pt)
  (foldl (lambda (p acc)
           (append acc (bezier-subtract p pt)))
         null
         from-pt))

; Bezier  Bezier ... -> (listof Bezier)
(define (join~ pt . pts)
  (if (null? pts)
      (list pt)
      (let* ([bb (bezier-bounding-box pt)]
             [overlaps 
              (filter (lambda (pi) 
                        (overlap-bounding-boxes?  bb (bezier-bounding-box pi))) 
                      pts)]
             [non-overlaps
              (set->list (set-subtract (list->set pts) (list->set overlaps)))])
        (if (null? overlaps)
            (append (list pt)
                    (apply join~ pts))
            (let ([j (bezier-union pt (car overlaps))])
              (if (= (length j) 1)
                  (apply join~ (car j) (append (cdr overlaps) non-overlaps))
                  (append (list (car overlaps))
                          (apply join~ pt (append (cdr overlaps) non-overlaps)))))))))


; Real, Real, Real, Real -> Bezier
; produce a rectangle (as a bezier curve) with lower left corner in (x, y) with width w and height h
(define (rect x y w h)
  (let ([x2 (+ x w)]
        [y2 (+ y h)])
    (~ (x y) -- (x2 y) -- (x2 y2) -- (x y2) -- (x y))))


; Real, Real, Real, Real -> Bezier
; produce an ellipse (as a bezier curve) with lower left corner (of the bounding box) in (x, y) with width w and height h
(define (ellipse x y w h)
  (let* ([x2 (+ x w)]
         [y2 (+ y h)]
         [xm (* (+ x x2) 0.5)]
         [ym (* (+ y y2) 0.5)]
         [t 0.551915])
    (~ (x2 ym) (x2 y2 t) (xm y2)
       (x y2 t) (x ym)
       (x y t) (xm y)
       (x2 y t) (x2 ym))))


; Real Real Real Real -> Bezier
; produce an arc with center (cx cy) radius r and angle a
(define (arc cx cy r a)
  (let* ([x1 (+ cx r)]
         [y2 (+ cy r)]
         [seg (~ (x1 cy) (x1 y2 0.551915) (cx y2))])
    (cond [(< a pi/2)
           (let-values ([(a b) (split seg (/ a pi/2))])
             a)]
          [(= a pi/2) seg]
          [(= a 2pi) (ellipse (- cx r) (- cy r) (* 2 r) (* 2 r))]
          [(> a 2pi) (error "arc: angle is greater than 2pi")]
          [(> a pi/2) (join-subpaths seg 
                                     (from (cx cy) 
                                           (rotate. (arc cx cy r (- a pi/2))
                                                    pi/2)))])))