#lang racket
(require "../../geometry.rkt"
       
         (for-syntax racket/list
                     syntax/parse))

(provide ~)

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
                           [((insert i) . r)
                            (p-lines #'r (datum->syntax 
                                          stx 
                                          (append (syntax->list acc) 
                                                  (list #'(insert i) #'(@ 0 0)))))]
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




(let ([b  (~ (10 10) -- (20 10) -- (20 20))])
    (~ (20 20) -- (-20 40) -- cycle))