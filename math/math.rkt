#lang racket
(require "flatfont.rkt"
         "ufo.rkt"
         "vec.rkt"
         "properties.rkt"
         "fontpict.rkt"
         "utilities.rkt")

(provide (except-out (all-from-out racket) + - * /)
         (rename-out [prod *]
                     [add +]
                     [sub -]
                     [div /]
                     [transform transform]
                     [translate translate]
                     [rotate rotate]
                     [scale scale]
                     [skew-x skew-x]
                     [skew-y skew-y]
                     [reflect-x reflect-x]
                     [reflect-y reflect-y]
                     [ffont->ufo flatfont->ufo]
                     [ffont-scale-glyphs glyphs-scale])
         (all-from-out "fontpict.rkt")
         (except-out (all-from-out "vec.rkt")
                     transform
                     translate
                     rotate
                     scale
                     skew-x
                     skew-y
                     reflect-x
                     reflect-y)
         x->
         y->
         define-fonts
         define-space
         code+expr
         write-font
         fix-components)


(define (contour+ c1 . cs)
  (struct-copy contour c1
               [points (apply map 
                              (lambda (p1 . ps)
                                (foldl vec+ (vec 0 0) (cons p1 ps)))
                              (contour-points c1)
                              (map contour-points cs))]))

(define (component+ c1 . cs)
  (struct-copy component c1
        [matrix (foldl (lambda (cc1 cc2) 
                         (match cc1
                           [(trans-mat x xy yx y xo yo)
                            (match cc2
                              [(trans-mat x2 xy2 yx2 y2 xo2 yo2)
                               (trans-mat (+ x x2) (+ xy xy2) (+ yx yx2) (+ y y2) (+ xo xo2) (+ yo yo2))])]))
                       (get-matrix c1)
                       (map get-matrix cs))]))

(define (anchor+ a1 . as)
  (struct-copy anchor a1
               [pos (foldl vec+ (get-position a1)
                           (map get-position as))]))

(define (glyph+ g1 . gs)
  (let [(gss (cons g1 gs))]
    (struct-copy fglyph g1
                 [advance 
                  (vec->list 
                   (foldl vec+ (vec 0 0)
                          (map (lambda (g) 
                                 (list->vec 
                                  (fglyph-advance g)))
                               gss)))]
                 [contours
                  (apply map contour+ (map fglyph-contours gss))]
                 [components
                  (apply map component+ (map fglyph-components gss))]
                 [anchors
                  (apply map anchor+ (map fglyph-anchors gss))])))


(define (font+ f1 . fs)
  (let [(fonts (cons f1 fs))]
  (struct-copy ffont f1
               [info (apply info+ (map ffont-info fonts))]
               [kerning (apply kerning+ (map ffont-kerning fonts))]
               [glyphs (apply map glyph+ (map ffont-glyphs fonts))])))

(define (info+ i1 . is)
  (letrec [(aux (lambda (i1 i2)
                  (dict-map i1
                            (lambda (key value)
                              (let [(v2 (dict-ref i2 key))]
                                (cons key
                                      (match value
                                        [(list _ ...) (map + value v2)]
                                        [(? number? value) (+ value v2)]
                                        [_ (error "cannot add info")])))))))]
    (foldl aux i1 is)))

(define (kerning+ k1 . ks)
  (letrec [(aux (lambda (k1 k2)
                  (map (lambda (kl1 kl2)
                         (cons (car kl1)
                               (map (lambda (kr1 kr2)
                                      (cons (car kr1)
                                            (+ (cdr kr1) (cdr kr2))))
                                    (cdr kl1) (cdr kl2))))
                       k1 k2)))]
    (foldl aux k1 ks)))

(define (font:* f s1 . ss)
  (scale f (apply * (cons s1 ss))))

(define (font:+ o1 . os)
  (apply (match o1
           [(ffont _ _ _ _) font+]
           [(fglyph _ _ _ _ _) glyph+]
           [(contour _ _) contour+]
           [(anchor _ _ _ _) anchor+]
           [(component _ _ _) component+])
         (cons o1 os)))

(define (font:- o1 . os)
  (apply font:+ (cons o1 (map (lambda (o) (font:* o -1))
                                  os))))

(define (font:/ o s1 . ss)
  (font:* o (apply * (map (lambda (s) 
                                (/ 1 s)) 
                              (cons s1 ss)))))


(define (add a . as)
  (match (cons a as)
    [(list (? ffont? _) ...)
     (apply font:+ a as)]
    [(list (? number? _) ...)
     (apply + a as)]
    [(list (? vec? _) ...)
     (foldl vec+ a as)]
    [_ (error "Invalid operands for product for addition")]))

(define (sub a . as)
  (match (cons a as)
    [(list (? ffont? _) ...)
     (apply font:+ a (map (lambda (i) (prod i -1)) as))]
    [(list (? number? _) ...)
     (apply - a as)]
    [(list (? vec? _) ...)
     (foldl vec+ a (map (lambda (i) (prod i -1)) as))]
    [_ (error "Invalid operands for product for addition")]))


(define (prod a . as)
  (match (cons a as)
    [(list-no-order (? ffont? f) (? number? s) ...)
     (apply font:* f s)]
    [(list-no-order (? vec? v) (? number? s) ...)
     (vec* v (apply * s))]
    [(list (? number? x) ...)
     (apply * x)]
    [_ (error "Invalid operands for product")]))

(define (div a . as)
  (match (cons a as)
    [(list-no-order (? ffont? f) (? number? s) ...)
     (apply font:* f (map (lambda (n) (/ 1.0 n)) s))]
    [(list-no-order (? vec? v) (? number? s) ...)
     (vec* v (apply * (map (lambda (n) (/ 1.0 n)) s)))]
    [(list (? number? x) ...)
     (apply / x)]
    [_ (error "Invalid operands for product")]))


;; PROJECTIONS

; T -> T
; project the object on the x axis (set every y coord. to zero)
(define (x-> o)
  (scale o 1 0))

; T -> T
; project the object on the y axis (set every x coord. to zero)
(define (y-> o)
  (scale o 0 1))  


(define (interpolables f . fs)
  (let ([f0 (foldl (lambda (f acc)
                     (let-values ([(a b) (compatible-fonts acc f)])
                       a))
                   f fs)])
    (cons f0 (map (lambda (f)
                    (let-values ([(a b) (compatible-fonts f f0)])
                      (match-fonts-contours f0 a)))
                  fs))))

(define-syntax-rule (define-fonts (id ...) (path ...))
  (define-values (id ...)
    (apply values
           (apply interpolables
                  (map (lambda (p)
                         (prepare-for-interpolation
                          (ufo->ffont (read-ufo p))
                          #f))
                       (list path ...))))))

(define-syntax (define-space stx)
  (syntax-case stx ()
    [(define-space id (origin [font ...]))
     (with-syntax ([(fname ...)
                    (map (lambda (f) 
                           (datum->syntax stx (string->symbol 
                                               (format "~a-~a" (syntax->datum #'id)
                                                       (syntax->datum f)))))
                         (syntax->list #'(font ...)))])
       #'(begin
           (define (id f . fs)
             (apply values (map (lambda (f) (add origin f)) (cons f fs))))
           (define fname (sub font origin)) ...))]))



;(define-syntax-rule (rot exp angle)
;  (let [(rad-angle (degree->rad angle))]
;    (parameterize ([current-transformation 
;                    (matrix-mul (rotation-matrix rad-angle)
;                                (current-transformation))])
;      (trans-eval exp))))

; Font, Font -> Font
; Produce a new font with components scale fields imported from f2
(define (fix-components f1 f2)
  (struct-copy ffont f1
               [glyphs (map (lambda (g1 g2)
                              (struct-copy fglyph g1
                                           [components (map import-component-scale
                                                            (fglyph-components g1)
                                                            (fglyph-components g2))]))
                            (ffont-glyphs f1)
                            (ffont-glyphs f2))]))
                                                      


(define (write-font f path #:round-coord [round-coord #f] #:format [format 2])
  (let ([rf (if round-coord 
                (with-precision (1) (font-round (ffont->ufo f)))
                (ffont->ufo f))])
    (write-ufo ((if (= format 2)
                        font->ufo2
                        font->ufo3)
                    rf)
                   path)))
  