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
         font)

; (~ (10 20) (-- x y) (10 20 0.2) (20 39) (-- 10 2) (100 50) (50 100) (120 200) (insert path))

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

(define -- '--)

(define insert 'insert)

(define rel 'rel)

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



(define (rect x y w h)
  (let ([x2 (+ x w)]
        [y2 (+ y h)])
    (~ (x y) (-- x2 y) (-- x2 y2) (-- x y2) (-- x y))))

(define (ellipse x y w h)
  (let* ([x2 (+ x w)]
         [y2 (+ y h)]
         [xm (* (+ x x2) 0.5)]
         [ym (* (+ y y2) 0.5)]
         [t 0.551915])
    (~ (xm y) (x2 y t) (x2 ym)
       (x2 y2 t) (xm y2)
       (x y2 t) (x ym)
       (x y t) (xm y))))

; change
(define (arc x y r a)
  (let ([x2 (+ x r)]
        [x3 (+ x (* r (cos a)))]
        [y3 (+ y (* r (sin a)))]
        [ym (+ y (* r (tan (* a 0.5))))])
    (~ (x y) (-- x2 y) (x2 ym 0.55) (x3 y3) (-- x y))))

;(glyph 'a 
;       ([a 0]
;        [b 100]
;        [c 500])
;       (/--/ 400)
;       [(~ (0 0) (30 30) ...)
;        (circle a b c c)])
;

(provide glyph
         /--/)

(define (/--/ n)
  (lambda (cnts)
    (values
     (ufo:advance n 0)
     (map ufo:bezier->contour cnts))))

(define (unicode name)
  (let ([ns (symbol->string name)])
    (if (> (string-length ns) 1)
        '()
        (list (char->integer (string-ref ns 0))))))

(define-syntax-rule (glyph name ([s v] ...)
                           (advance-op advance-v ...)
                           (contour ...))
  (let* ([s v] ...)
    (let-values ([(adv cnts)
                  ((advance-op advance-v ...) (list contour ...))])
      (ufo:glyph 1 name adv (unicode name) #f #f '() '() cnts '() #f))))
             



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
       
       
(define (alg al)
  (car al))

(define (ovs al)
  (+ (alg al) (ovs-height al)))

(define (ovs-height al)
  (cadr al))

(define-syntax-rule (font (name upm ascender param ...)
                          (alignments [n al ovs] ...)
                          (variables v ...)
                          glyph ...)
  (define (name param ...)
    (let* ([n (list al ovs)] ...
           v ...)
      (ufo:font 2 "ufo-rkt"
                (make-immutable-hash
                 (list (cons 'unitsPerEm upm)
                       (cons 'ascender ascender)
                       (cons 'descender (- ascender upm))
                       (cons 'familyName (symbol->string (quote name)))
                       (cons 'postscriptFontName (symbol->string (quote name)))
                       (cons 'versionMajor 1)
                       (cons 'versionMinor 0)))
                #f #f #f
                (list 
                 (ufo:layer 'public.default #f
                            (list glyph ...)))
                #f #f #f))))

(font (squarefont 1000 750 [x-height 500] [width 0.5] [weight 0.5]) 
        (alignments
         [base 0 -10]
         [asc 720 12]
         [xh x-height 10]
         [dsc -200 -10])
        (variables
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
               [(rect x1 y1 v-stem (alg asc))
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
                (rect (+ x1 gw (- v-stem)) y1 v-stem (alg asc))])
        (glyph 'o
               ()
               (/--/ (+ gw space space))
               [(rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))])
        )

               