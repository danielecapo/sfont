#lang racket
(require "vec.rkt"
         "bezier.rkt"
         (prefix-in ufo: "glif.rkt")
         (prefix-in ufo: "font.rkt")
         "utilities.rkt"
         "fontpict.rkt")

(set-sample-size! 150)

(provide ~
         rect
         ellipse
         arc
         glyph
         /--/
         alg
         ovs
         ovs-height
         font
         from
         (all-from-out "bezier.rkt")
         (except-out (all-from-out "vec.rkt")
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

; (~ (10 20) (-- x y) (10 20 0.2) (20 39) (-- 10 2) (100 50) (50 100) (120 200) (insert path))
#;
(define-syntax-rule (~ (c v ...) ...)
  (process-path (foldl (lambda (pe p)
                         (append p 
                                 (if (eq? (car pe) insert)
                                     (let ([ips (map vec->list (cadr pe))])
                                       (cons (cons -- (first ips))
                                             (cdr ips)))
                                     (list pe))))
                       '()
                       (list (list c v ...) ...))))



(define-syntax ~
  (syntax-rules (-- insert)
    [(~) '()]
    [(~ (x y) (insert vlist) . r)
     (join-subpaths (list (vec x y))
                    (~ (insert vlist) . r))]
   
    [(~ (insert vlist) . r)
     (join-subpaths vlist (~ . r))]
    [(~ (x y)) (list (vec x y))]
    [(~ (x y) (-- x1 y1) . r)
     (append (list (vec x y) (vec x y) (vec x1 y1))
             (~ (x1 y1) . r))]
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

; join-subpaths
; BezierCurve, BezierCurve -> BezierCurve
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

    



; process-path
; path -> bezier curve
; produce a bezier vurve from instruction in 'path'
#;
(define (process-path pth)
  (letrec ([aux (lambda (pth acc)
                  (match pth
                    [(list p) acc]
                   
                    [(list-rest `(,x ,y) `(-- ,x ,y) p)
                     (aux (cons `(,x ,y) p)
                          acc)]
                    [(list-rest `(,x ,y) `(-- ,x1 ,y1) p)
                     (aux (cons `(,x1 ,y1) p)
                          (append acc (list (vec x y) (vec x1 y1) (vec x1 y1))))]
                    [(list-rest `(,x1 ,y1) `(,cx1 ,cy1) `(,cx2 ,cy2) `(,x2 ,y2) p)
                     (aux (cons `(,x2 ,y2) p)
                          (append acc (list (vec cx1 cy1) (vec cx2 cy2) (vec x2 y2))))]
                    [(list-rest `(,x1 ,y1) `(,cx ,cy ,t) `(,x2 ,y2) p)
                     (aux (cons `(,x2 ,y2) p)
                          (append acc (list (vec+ (vec x1 y1) (vec* (vec- (vec cx cy) (vec x1 y1)) t)) 
                                            (vec+ (vec x2 y2) (vec* (vec- (vec cx cy) (vec x2 y2)) t))
                                            (vec x2 y2))))]
                    [(list-rest `(,x1 ,y1) `(,cx ,cy ,t1 ,t2) `(,x2 ,y2) p)
                     (aux (cons `(,x2 ,y2) p)
                          (append acc (list (vec+ (vec x1 y1) (vec* (vec- (vec cx cy) (vec x1 y1)) t1)) 
                                            (vec+ (vec x2 y2) (vec* (vec- (vec cx cy) (vec x2 y2)) t2))
                                            (vec x2 y2))))]
                   
                    ))])
    (aux pth (list (vec (caar pth) (cadar pth))))))


; rect
; Number, Number, Number, Number -> bezier
; produce a rectangle (as a bezier curve) with lower left corner in (x, y) with width w and height h
(define (rect x y w h)
  (let ([x2 (+ x w)]
        [y2 (+ y h)])
    (~ (x y) (-- x2 y) (-- x2 y2) (-- x y2) (-- x y))))

; rect
; Number, Number, Number, Number -> bezier
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

(define pi/2 (/ pi 2))
(define pi/3 (/ pi 3))
(define pi/4 (/ pi 4))
(define pi/6 (/ pi 6))
(define 2pi (* 2 pi))



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

(define-syntax-rule (define-transform name fn)
  (define-syntax name
    (syntax-rules (from)
      [(name o from (x y) . args)
       (from (x y) (name o . args))]
      [(name o . args)
       (cond [(list? o) (map (lambda (i) (fn i . args)) o)]
             [else (fn o . args)])]
      )))


(define (translate- o x y) (translate o (vec x y)))
(define-transform translate* translate-)
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
;       ([a 0]
;        [b 100]
;        [c 500])
;       (/--/ 400)
;       [(~ (0 0) (30 30) ...)
;        (circle a b c c)])
;

; /--/
; Number -> procedure
; produce a procedure that calculate the advance width and transform beziers in ufo contours

(define (/--/ n)
  (lambda (cnts)
    (values
     (ufo:advance n 0)
     (map ufo:bezier->contour cnts))))

; unicode
; Symbol -> Number
; produce the unicode code of the glyph
; note: this has to be changed to deal with common names

(define (unicode name)
  (let ([ns (symbol->string name)])
    (if (> (string-length ns) 1)
        '()
        (list (char->integer (string-ref ns 0))))))

(define-syntax glyph
  (syntax-rules (contours)
    [(glyph name ([s v] ...)
            (advance-op advance-v ...)
            [contours contour ...])
     (let* ([s v] ...)
       (let-values ([(adv cnts)
                     ((advance-op advance-v ...) (build-contour-list contour ...))])
         (ufo:glyph 1 name adv (unicode name) #f #f '() '() cnts '() #f)))]
    
    ))
 
(define (build-contour-list . cnts)
  (foldl (lambda (c r)
           (append r (if (andmap vec? c)
                         (list c)
                         c)))
         '()
         cnts))



;    [(with-transformation [(tfn . args) (rtfn . rargs) ...] cnt ...)
;     (with-transformation [(rtfn . rargs) ...] . (with-transformation [(tfn . args)] cnt ...))]))

;(font (augusto 1000 720 [weight 0.5] [width 0.5])
;      (alignments
;       [a 0 -10]
;       [b (* width 100) 10])
;      (variables
;       [k (* a 2)])
;      (glyph 'A
;         ([a 0]
;          [b 100]
;          [c 500])
;         (/--/ 500)
;         [(ellipse a b c c)])
;      ...)
       
; Alignemnt is a pair, the first element is the position, the second represents the height of overshoot

; alg 
; Alignment -> Number
; return the position of alignment

(define (alg al)
  (car al))


; ovs
; Alignment -> Number
; return the position of overshoot for Alignment

(define (ovs al)
  (+ (alg al) (ovs-height al)))

; ovs-height
; Alignment -> Number
; return the height of overshoot for Alignment

(define (ovs-height al)
  (cadr al))
#;
(define-syntax build-alignments
  (syntax-rules (-is-ascender)
    [(build-alignments ()) ()]
    [(build-alignments ([a al ov] . r)) ([a (list al ov)] . ((build-alignments r)))]
    [(build-alignments ([a al ov -is-ascender] . r)) ([a (list al ov)] . ((build-alignments r)))]))


#;
(define-syntax font
  (syntax-rules (alignments variables :use-as-ascender)
    [(font (name upm param ...)
           (alignments [n al ovs . r] ...)
           (variables v ...)
           glyph ...)
     (define (name param ...)
       (let* ([n (list al ovs)] ...
             ; [ascender (list asc-al asc-ovs)]
              v ...)
         (ufo:font 2 "ufo-rkt"
                   (make-immutable-hash
                    (list (cons 'unitsPerEm upm)
                         ; (cons 'ascender asc-al)
                         ; (cons 'descender (- asc-al upm))
                          (cons 'familyName (symbol->string (quote name)))
                          (cons 'postscriptFontName (symbol->string (quote name)))
                          (cons 'versionMajor 1)
                          (cons 'versionMinor 0)))
                   #f #f #f
                   (list 
                    (ufo:layer 'public.default #f
                               (list glyph ...)))
                   #f #f #f)))]))


(define-syntax emit-font-form
  (syntax-rules ()
    [(emit-font-form name ascender-id descender-id
                     (v ...)
                     glyph ...)
     
       (let* (v ...)
         (ufo:font 2 "ufo-rkt"
                   (make-immutable-hash
                    (list (cons 'unitsPerEm (+ (alg ascender-id) (- (alg descender-id))))
                          (cons 'ascender (alg ascender-id))
                          (cons 'descender (alg descender-id))
                          (cons 'familyName (symbol->string (quote name)))
                          (cons 'postscriptFontName (symbol->string (quote name)))
                          (cons 'versionMajor 1)
                          (cons 'versionMinor 0)))
                   #f #f #f
                   (list 
                    (ufo:layer 'public.default #f
                               (build-list glyph ...)))
                   #f #f #f))]))


(define (build-list . glyphs)
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

(define-syntax font 
  (syntax-rules (alignments variables)
    [(font (name params ...) . rest)
     (kw-args-lambda (params ...) () (font name . rest))]
    [(font name
       (alignments als ...)
       (variables v ...)
       glyph ...)
     (let-alignment (als ...)
                    (emit-font-form name 
                                    (find-ascender (als ...)) 
                                    (find-descender (als ...)) 
                                    (v ...) 
                                    glyph ...))]
    [(font name
       (alignments als ...)
       glyph ...)
     (let-alignment (als ...)
                    (emit-font-form name 
                                    (find-ascender (als ...)) 
                                    (find-descender (als ...)) 
                                    () 
                                    glyph ...))]))

;     (step-1 name
;             (als ...)
;             (v ...)
;             (glyph ...)
;             ()
;             ()
;       )]))

;     (step-1 (name upm param ...)
;             ()
;             ()
;             (als ...)
;             (v ...)
;             glyph ...)]))

#;
(define-syntax step-1
  (syntax-rules (:use-as-ascender :use-as-descender)
    [(step-1 name
             ([n a o] . als)
             (v ...)
             (glyph ...)
             . r)
     (let ([n (list a o)])
       (step-1 name
               als
               (v ...)
               (glyph ...)
               . r))]
    
    [(step-1 name
             ([n a o :use-as-ascender] . als)
             (v ...)
             (glyph ...)
             as
             desc)
     (let ([n (list a o)])
       (step-1 name
               als
               (v ...)
               (glyph ...)
               n
               desc))]
    [(step-1 name
             ([n a o :use-as-descender] . als)
             (v ...)
             (glyph ...)
             as
             desc)
     (let ([n (list a o)])
       (step-1 name
               als
               (v ...)
               (glyph ...)
               asc
               n))]
    [(step-1 name
             ()
             (v ...)
             (glyph ...)
             asc
             desc)
     (emit-font-form name asc desc (v ...) glyph ...)]))



#;
(define-font (squarefont 1000  asc* [x-height 500] [width 0.5] [weight 0.5]) 
        (alignments
         [base 0 -10]
         [xh x-height 10]
         [dsc -200 -10]
         [ascender 740 10]
         [asc* 750 0])
        (
         [gw (* 1000 width)]
         [v-stem (* x-height weight 0.333)]
         [h-stem (* v-stem 0.9)]
         [space (/ (- gw (* 2 v-stem)) 2)]
         [x1 space]
         [y1 (alg base)]
         [ym (/ x-height 2)]
         [x2 (+ space gw (- v-stem))])
        (glyph 'a
               ()
               (/--/ (+ gw space space))
               [(rect x1 y1 gw h-stem)
                (rect x1 y1 v-stem ym)
                (rect x1 (- ym (/ h-stem 2)) gw h-stem)
                (rect x2 y1 v-stem x-height)
                (rect x1 (- x-height h-stem) gw h-stem)])
        (glyph 'b
               ()
               (/--/ (+ gw space space))
               [(rect x1 y1 v-stem (alg ascender))
                (rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))])
        (glyph 'c
               ([term ym])
               (/--/ (+ gw space space))
               [(rect x1 y1 v-stem x-height)
                (rect x1 (- x-height h-stem) gw h-stem)
                (rect x1 y1 gw h-stem)
                (rect (+ x1 gw (- v-stem)) (- x-height term) v-stem term)])
        (glyph 'd
               ()
               (/--/ (+ gw space space))
               [(rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))
                (rect (+ x1 gw (- v-stem)) y1 v-stem (alg ascender))])
        (glyph 'o
               ()
               (/--/ (+ gw space space))
               [(rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))])
        )


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
      
