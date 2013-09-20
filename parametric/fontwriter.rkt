#lang racket
(require "../geometry.rkt"
         "../ufo.rkt"
         "../utilities.rkt"
         "../fontpict.rkt"
         "../glyphlist.rkt"
         "../spacing/space.rkt")

(set-sample-size! 150)

(provide ~
         rect
         ellipse
         arc
         glyph.
         alg
         ovs
         ovs-height
         font.
         from
         (except-out (all-from-out "../geometry.rkt")
                     translate
                     rotate
                     scale
                     skew-x
                     skew-y
                     reflect-x
                     reflect-y)
         (rename-out [translate* translate]
                     [rotate* rotate]
                     [scale* scale]
                     [skew-x* skew-x]
                     [skew-y* skew-y]
                     [reflect-x* reflect-x]
                     [reflect-y* reflect-y]))

;(define-syntax ~
;    (syntax-rules (insert cycle)
;      [(~ (o ...) ())
;       (path o ...)]
;      [(~ ((x y) p ...) (o cycle))
;       (~ ((x y) p ... o (x y)) ())]
;      [(~ (p ...) (insert . r))
;       (path p ... (insert . r))]
;      [(~ (p ...) (o . r))
;       (~ (p ... o) r)]
;      [(~ (x y) . r)
;       (~ ((x y)) r)]))


(define-syntax ~
  (syntax-rules (-- insert @ cycle)
    [(~ (insert vlist) c ... cycle)
     (let ([fp (car vlist)])
       (~ (insert vlist) c ... ((vec-x fp) (vec-y fp))))]
    [(~ (x y) c ... cycle)
     (~ (x y) c ... (x y))]
    [(~) '()]     
    [(~ (insert vlist) (@ insert vlist2) . r)
     (let ([sb vlist])
       (join-subpaths vlist
                      (~ (insert (translate* vlist2 
                                             (vec-x (last vlist)) 
                                             (vec-y (last vlist))))
                         . r)))]
    [(~ (x y) (@ insert vlist) . r)
     (~ (x y) (insert (translate* vlist x y)) . r)]
    [(~ (x y) (insert vlist) (@ x1 y1) . r)
     (let ([lasti (last vlist)])
       (~ (x y) 
          (insert vlist) 
          ((+ (vec-x lasti) x1)
           (+ (vec-y lasti) y1))
          . r))]
    [(~ (x y) (insert vlist) . r)
     (join-subpaths (list (vec x y))
                    (~ (insert vlist) . r))]
    [(~ (insert vlist) . r)
     (join-subpaths vlist (~ . r))]
    [(~ (x y)) (list (vec x y))]
    [(~ (x y) -- (x1 y1) . r)
     (let ([v (vec x y)]
           [v1 (vec x1 y1)])
       (append (list v v v1)
             (~ ((vec-x v1) (vec-y v1)) . r)))]
    [(~ (x y) -- (@ x1 y1) . r)
     (~ (x y) -- ((+ x1 x) (+ y1 y)) . r)]
    [(~ (x y) (@ x1 y1 . a) . r)
     (~ (x y) ((+ x1 x) (+ y1 y) . a) . r)]
    [(~ (x y) (x1 y1 . a) (@ x2 y2 . a2) . r)
     (~ (x y) (x1 y1 . a) ((+ x2 x1) (+ y2 y1) . a2) . r)]
    [(~ (x y) (x1 y1 . a) (x2 y2 . a2) (@ x3 y3 . a3) . r)
     (~ (x y) (x1 y1 . a) (x2 y2 . a2) ((+ x3 x2) (+ y3 y2) . a3) . r)]
    [(~ (x y) (cx cy) (cx1 cy1) (x1 y1) . r)
     (append (list (vec x y) (vec cx cy) (vec cx1 cy1))
             (~ (x1 y1) . r))]
    [(~ (x y) (cx cy t) (x1 y1) . r)
     (~ (x y) (cx cy t t) (x1 y1) . r)]
    [(~ (x y) (cx cy t t1) (x1 y1) . r)
     (append (list (vec x y) 
                   (vec+ (vec x y) (vec* (vec- (vec cx cy) (vec x y)) t)) 
                   (vec+ (vec x1 y1) (vec* (vec- (vec cx cy) (vec x1 y1)) t1)))
             (~ (x1 y1) . r))]))



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

 

; Number, Number, Number, Number -> Bezier
; produce a rectangle (as a bezier curve) with lower left corner in (x, y) with width w and height h
(define (rect x y w h)
  (let ([x2 (+ x w)]
        [y2 (+ y h)])
    (~ (x y) -- (x2 y) -- (x2 y2) -- (x y2) -- (x y))))


; Number, Number, Number, Number -> Bezier
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


; Number Number Number Number -> Bezier
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
                                           (rotate* (arc cx cy r (- a pi/2))
                                                    pi/2)))])))

; Syntax for defining geometric transformations
(define-syntax-rule (define-transform name fn)
  (define-syntax name
    (syntax-rules (from)
      [(name o from (x y) . args)
       (from (x y) (name o . args))]
      [(name o . args)
       (cond [(list? o) (map (lambda (i) (fn i . args)) o)]
             [else (fn o . args)])]
      )))



(define-transform translate* translate)
(define-transform rotate* rotate)
(define-transform scale* scale)
(define-transform skew-x* skew-x)
(define-transform skew-y* skew-y)
(define-transform reflect-x* reflect-x)
(define-transform reflect-y* reflect-y)


(define-syntax-rule (from (x y) (fn o . args))
  (translate* (fn (translate* o (- x) (- y)) . args)
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


; Symbol -> Number
; produce the unicode code of the glyph using adobe glyph list
(define (unicode name)
  (hash-ref adobe-glyph-list name '()))

(define-syntax glyph.
  (syntax-rules (metrics contours locals)
    [(glyph. name 
            (metrics metric-form ...)
            [contours contour ...])
     (glyph. name (locals)
            (metrics metric-form ...)
            [contours contour ...])]
    [(glyph. name (locals [s v] ...)
            (metrics metric-form ...)
            [contours contour ...])
     (let* ([s v] ...)
       (space-glyph
        #f
        (glyph 1 name (advance 0 0) (unicode name) #f #f '() '() 
                   (map bezier->contour (build-contour-list contour ...)) '() #f)
         metric-form ...))]))

;remove?
;(glyph-metric
;         (glyph 1 name (advance 0 0) (unicode name) #f #f '() '() 
;                    (map bezier->contour (build-contour-list contour ...)) '() #f)
;         metric-form ...))]))

(define-syntax glyph-metric
  (syntax-rules (/--/ /<- ->/)
    [(glyph-metric g (/--/ a))
     (struct-copy glyph g
                  [advance (advance a 0)])]
    [(glyph-metric g (/<- l) (->/ r))
     (glyph-metric 
      (glyph-metric g (/<- l))
      (->/ r))]
    [(glyph-metric g (/<- l))
     (let ([sr (cdr (get-sidebearings #f g))])
       (set-sidebearings #f g l sr))]
    [(glyph-metric g (->/ r))
     (let ([sl (car (get-sidebearings #f g))])
       (set-sidebearings #f g sl r))]
    [(glyph-metric g (/<- l lm) (->/ r rm))
     (glyph-metric 
      (glyph-metric g (/<- l lm))
      (->/ r rm))]
    [(glyph-metric g (/<- l) (->/ r rm))
     (glyph-metric 
      (glyph-metric g (/<- l))
      (->/ r rm))]
    [(glyph-metric g (/<- l lm) (->/ r))
     (glyph-metric 
      (glyph-metric g (/<- l lm))
      (->/ r))]
    [(glyph-metric g (/<- l lm))
     (let ([sr (cdr (get-sidebearings-at #f g lm))])
       (set-sidebearings-at #f g l sr lm))]
    [(glyph-metric g (->/ r rm))
     (let ([sl (car (get-sidebearings-at #f g rm))])
       (set-sidebearings-at #f g sl r rm))]
    [(glyph-metric g (/<- l) (/--/ a))
     (glyph-metric (set-sidebearings #f g l 0) (/--/ a))]
    [(glyph-metric g (/<- l lm) (/--/ a))
     (glyph-metric (set-sidebearings-at #f g l 0 lm) (/--/ a))]
    [(glyph-metric g (/--/ a) (->/ r))
     (let* ([sb (get-sidebearings #f g)]
            [sl (car sb)]
            [sr (cdr sb)])
       (glyph-metric g (/<- (+ a sl sr (- r))) (/--/ a)))]
    [(glyph-metric g (/--/ a) (->/ r rm))
     (let* ([sb (get-sidebearings-at #f g rm)]
            [sl (car sb)]
            [sr (cdr sb)])
       (glyph-metric g (/<- (+ a sl sr (- r)) rm) (/--/ a)))]))
     

; (Bezier or (listOf Bezier) ... -> (listOf Bezier)
(define (build-contour-list . cnts)
  (if (not (car cnts))
      '()
      (foldl (lambda (c r)
               (append r (if (andmap vec? c)
                             (list c)
                             c)))
             '()
             cnts)))




       
; Alignemnt is a pair, the first element is the position, the second represents the height of overshoot


; Alignment -> Number
; return the position of alignment
(define (alg al)
  (car al))



; Alignment -> Number
; return the position of overshoot for Alignment
(define (ovs al)
  (+ (alg al) (ovs-height al)))


; Alignment -> Number
; return the height of overshoot for Alignment
(define (ovs-height al)
  (cadr al))


(define-syntax emit-font-form
  (syntax-rules ()
    [(emit-font-form name ascender-id descender-id
                     (v ...)
                     glyph ...)
     
       (let* (v ...)
         (font 2 "ufo-rkt"
                   (make-immutable-hash
                    (list (cons 'unitsPerEm (+ (alg ascender-id) (- (alg descender-id))))
                          (cons 'ascender (alg ascender-id))
                          (cons 'descender (alg descender-id))
                          (cons 'familyName (symbol->string (quote name)))
                          (cons 'postscriptFontName (symbol->string (quote name)))
                          (cons 'versionMajor 1)
                          (cons 'versionMinor 0)))
                   (make-immutable-hash) 
                   (make-immutable-hash) 
                   #f
                   (list 
                    (layer 'public.default #f
                               (build-glyphs-list glyph ...)))
                   (make-immutable-hash)
                   #f #f))]))


; (Glyph or (listOf Glyph)) ... -> (listOf Glyph)
(define (build-glyphs-list . glyphs)
  (foldl (lambda (g r) (if (list? g) 
                           (append r g)
                           (append r (list g))))
         '()
         glyphs))

(define-syntax (kw-args-lambda stx)
  (syntax-case stx ()
    [(kw-args-lambda () (kwargs ...) font-form)
     #'(lambda (kwargs ...) font-form)]
    [(kw-args-lambda ([n v] . args) kwargs font-form)
     (with-syntax ([kw (datum->syntax stx 
                                      (string->keyword (symbol->string (syntax->datum #'n))))])
       #'(kw-args-lambda args 
                       (kw . ([n v] . kwargs))
                       font-form))]))

(define-syntax font. 
  (syntax-rules (alignments variables glyphs)
    [(font. (name params ...) . rest)
     (kw-args-lambda (params ...) () (font name . rest))]
    [(font. name
       (alignments als ...)
       (variables v ...)
       (glyphs glyph ...))
     (let-alignment (als ...)
                    (emit-font-form name 
                                    (find-ascender (als ...)) 
                                    (find-descender (als ...)) 
                                    (v ...) 
                                    glyph ...))]
    [(font. name
       (alignments als ...)
       glyph ...)
     (let-alignment (als ...)
                    (emit-font-form name 
                                    (find-ascender (als ...)) 
                                    (find-descender (als ...)) 
                                    () 
                                    glyph ...))]))



(define-syntax (let-alignment stx)
    (syntax-case stx ()
      [(let-alignment ([n a o . r] ...) emit-form)
       #'(let* ([n (list a o)] ...) emit-form)]))


(define-syntax (find-ascender stx)
    (syntax-case stx (:use-as-ascender)
      [(find-ascender ())
       (error "font doesn't define an alignment to be used as ascender in fontinfo")]
      [(find-ascender ([n a o :use-as-ascender] . as))
       #'n]
      [(find-ascender ([n a o . r] . as))
       #'(find-ascender as)]))

(define-syntax (find-descender stx)
    (syntax-case stx (:use-as-descender)
      [(find-descender ())
       (error "font doesn't define an alignment to be used as ascender in fontinfo")]
      [(find-descender ([n a o :use-as-descender] . as))
       #'n]
      [(find-descender ([n a o . r] . as))
       #'(find-descender as)]))
      
