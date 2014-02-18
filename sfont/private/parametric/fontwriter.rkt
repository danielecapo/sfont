#lang racket
(require "../../geometry.rkt"
         "../../main.rkt"
         "../../utilities.rkt"
         "../spacing/space.rkt"
         (only-in "../syntax-keywords.rkt" @° insert >< cycle metrics contours locals alignments variables glyphs :font-ascender :font-descender)
         (for-syntax racket/list
                     syntax/parse))


(provide 
 (contract-out
  [alignment/c (-> any/c boolean?)]
  [rect (-> real? real? real? real? cubic-bezier/c)]
  [ellipse (-> real? real? real? real? cubic-bezier/c)]
  [arc (-> real? real? real? real? cubic-bezier/c)]
  [alg (-> alignment/c real?)]
  [ovs (-> alignment/c real?)]
  [ovs-height (-> alignment/c real?)]
  [remove~ (->* (and/c closed-bezier/c cubic-bezier/c) () #:rest (listof (and/c closed-bezier/c cubic-bezier/c)) 
                (listof (and/c closed-bezier/c cubic-bezier/c)))]
  [join~ (->* (and/c closed-bezier/c cubic-bezier/c) () #:rest (listof (and/c closed-bezier/c cubic-bezier/c)) 
              (listof (and/c closed-bezier/c cubic-bezier/c)))])
 glyph.
 font.
 from
 ~
 translate.
 rotate.
 scale.
 skew-x.
 skew-y.
 reflect-x.
 reflect-y.
 )

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
             (error #f "Angles don't converge.")
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
                           #:datum-literals (--)
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

; Syntax for defining geometric transformations
(define-syntax-rule (define-transform name fn)
  (define-syntax name
    (syntax-rules (from)
      [(name o from (x y) . args)
       (from (x y) (name o . args))]
      [(name o . args)
       (cond [(list? o) (map (lambda (i) (fn i . args)) o)]
             [else (fn o . args)])])))



(define-transform translate. translate)
(define-transform rotate. rotate)
(define-transform scale. scale)
(define-transform skew-x. skew-x)
(define-transform skew-y. skew-y)
(define-transform reflect-x. reflect-x)
(define-transform reflect-y. reflect-y)


(define-syntax-rule (from (x y) (fn o . args))
  (translate. (fn (translate. o (- x) (- y)) . args)
              x y))




;(glyph 'a 
;       (locals 
;        [a 0]
;        [b 100]
;        [c 500])
;       (metrics (/--/ 400))
;       [contours (~ (0 0) (30 30) ...)
;                 (circle a b c c)])
;




(define-syntax (glyph. stx)
  (syntax-case stx (metrics contours locals -- <-> /--/)
    [(glyph. name 
             [metrics left-form right-form]
             . contour-form)
     #'(glyph. name 
               [locals]
               [metrics left-form right-form]
               . contour-form)]
    [(glyph. name 
             [locals [s v] ...]
             [metrics left-form right-form]
             . contour-form)
     (with-syntax ([cnts (syntax-case #'contour-form (contours)
                           [() #'(list)]
                           [((contours cnt . cnts))
                            #'(map bezier->contour 
                                   (build-contour-list cnt . cnts))])])
       #'(let* ([s v] ...)
           (space-glyph
            (glyph name (advance 0 0) (unicode name) #f #f 
                   (list (layer foreground null null cnts null))
                   (make-immutable-hash))
            left-form right-form)))]
    [(glyph. . body) (raise-syntax-error #f "Invalid glyph. definition." stx)]))



; (Bezier or (listOf Bezier) ... -> (listOf Bezier)
(define/contract (build-contour-list . cnts)
  (->* () () #:rest (listof (or/c bezier/c (listof bezier/c))) (listof bezier/c))
  (foldl 
   (lambda (c r)
     (append r (if (bezier/c c)
                   (list c)
                   c)))
   '()
   cnts))




; Alignemnt is a list of two elements, the first element is the position, the second represents the height of overshoot
(define alignment/c (flat-named-contract 'alignment/c (list/c real? real?)))

; Alignment -> Real
; return the position of alignment
(define (alg al)
  (car al))



; Alignment -> Real
; return the position of overshoot for Alignment
(define (ovs al)
  (+ (alg al) (ovs-height al)))


; Alignment -> Real
; return the height of overshoot for Alignment
(define (ovs-height al)
  (cadr al))


(define-syntax emit-font-form
  (syntax-rules ()
    [(emit-font-form name 
                     ascender-id 
                     descender-id
                     (blue ...)
                     (v ...)
                     glyph-form ...)
     
     (let* (v ...)
       (font (make-immutable-hash
              (let* ([all-blues (list (list (alg blue) (ovs blue)) ...)]
                     [blues (sort (flatten 
                                   (filter ((curry ormap) 
                                            (negate negative?)) 
                                           all-blues))
                                  <)]
                     [o-blues (sort (flatten 
                                     (filter ((curry andmap) negative?)
                                             all-blues))
                                    <)]
                     [infoa (list (cons 'unitsPerEm (+ (alg ascender-id) (- (alg descender-id))))
                                  (cons 'ascender (alg ascender-id))
                                  (cons 'descender (alg descender-id))
                                  (cons 'familyName (symbol->string (quote name)))
                                  (cons 'postscriptFontName (symbol->string (quote name)))
                                  (cons 'versionMajor 1)
                                  (cons 'versionMinor 0))]
                     [infob (if (null? blues)
                                infoa
                                (cons (cons 'postscriptBlueValues blues)
                                      infoa))])
                (if (null? o-blues)
                    infob
                    (cons (cons 'postscriptBlueValues blues)
                          infob))))
             (make-immutable-hash) 
             (make-immutable-hash) 
             #f
             (build-glyphs-list glyph-form ...)
             (list 
              (layer-info foreground #f (make-immutable-hash)))
             (make-immutable-hash)
             #f #f))]))


; (Glyph or (listOf Glyph)) ... -> (listOf Glyph)
(define/contract (build-glyphs-list . glyphs)
  (->* () () #:rest (listof (or/c glyph? (listof glyph?))) (listof glyph?))
  (foldl (lambda (g r) (if (list? g) 
                           (append r g)
                           (append r (list g))))
         '()
         glyphs))



(define-syntax (font. stx)
  (syntax-case stx (alignments variables glyphs)
    [(font. (name [param dflt] ...) . rest)
     (for-each (lambda (p) 
                 (unless (identifier? p)
                   (raise-syntax-error #f "Expected identifier" stx p)))
               (syntax->list #'(param ...)))
     (with-syntax ([kwarglist
                    (datum->syntax stx
                                   (append*
                                    (map (lambda (p d)
                                           (cons (string->keyword (symbol->string (syntax->datum p))) (list (list p d))))
                                         (syntax->list #'(param ...))
                                         (syntax->list #'(dflt  ...)))))])
       #'(lambda kwarglist (font. name . rest)))]
    [(font. name
            [alignments als ...]
            [variables v ...]
            [glyphs glyph-form ...])
     (for-each (lambda (i) (unless (identifier? i)
                             (raise-syntax-error #f "Expected identifier" stx #'i)))
               (cons #'name
                     (append (map (compose car syntax->list) (syntax->list #'(als ...)))
                             (map (compose car syntax->list) (syntax->list #'(v ...))))))
     (letrec ([find-blues
               (lambda (s acc)
                 (syntax-case s (:font-ascender :font-descender)
                   [() acc]
                   [([n a o :font-ascender] . as) (find-blues #'as acc)]
                   [([n a o :font-descender] . as) (find-blues #'as acc)]
                   [([n a o] . as) (find-blues #'as (datum->syntax s (cons #'n (syntax->list acc))))]))]
              [find-ascender
               (lambda (s)
                 (syntax-case s (:font-ascender)
                   [() (raise-syntax-error #f "Font doesn't define an alignment to be used as ascender in fontinfo" #'(alignments als ...))]
                   [([n a o :font-ascender] . as) #'n]
                   [([n a o] . as) (find-ascender #'as)]
                   [([n a o r] . as) (find-ascender #'as)]))]
              [find-descender
               (lambda (s)
                 (syntax-case s (:font-descender)
                   [() (raise-syntax-error #f "Font doesn't define an alignment to be used as descender in fontinfo" #'(alignments als ...))]
                   [([n a o :font-descender] . as) #'n]
                   [([n a o . r] . as) (find-descender #'as)]))])
       (with-syntax ([ascender  (find-ascender  #'(als ...))]
                     [descender (find-descender #'(als ...))]
                     [((alg-name a o . r) ...) #'(als ...)]
                     [(blue ...) (find-blues #'(als ...) #'())])
         #'(let* ([alg-name (list a o)] ...)
             (emit-font-form name 
                             ascender 
                             descender 
                             (blue ...)
                             (v ...) 
                             glyph-form ...))))]
    [(font. name
            [alignments als ...]
            [glyphs glyph-form ...])
     #'(font. name [alignments als ...] [variables] [glyphs glyph-form ...])]))

