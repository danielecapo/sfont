#lang racket
(require "flatfont.rkt"
         "../ufo.rkt"
         "../geometry.rkt"
         "../properties.rkt"
         "../utilities.rkt")

(provide (except-out (all-from-out racket) + - * /)
         (rename-out [transform transform]
                     [translate translate]
                     [rotate rotate]
                     [scale scale]
                     [skew-x skew-x]
                     [skew-y skew-y]
                     [reflect-x reflect-x]
                     [reflect-y reflect-y]
                     [ffont->ufo get-ufo]
                     [ffont-scale-glyphs glyphs-scale])
         (except-out (all-from-out "../geometry.rkt")
                     transform
                     translate
                     rotate
                     scale
                     skew-x
                     skew-y
                     reflect-x
                     reflect-y)
         (contract-out
          [ffont? (-> any/c boolean?)]
          [fglyph? (-> any/c boolean?)]
          [fontmath-object/c (-> any/c boolean?)]
          [font-object/c (-> any/c boolean?)]
          [get-interpolable-fonts (->* () () #:rest (listof font?) (listof ffont?))]
          [rename prod * (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [rename add  + (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [rename sub  - (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [rename div  / (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [x-> (-> font-object/c font-object/c)]
          [y-> (-> font-object/c font-object/c)]
          [fix-components (-> ffont? ffont? ffont?)])
         define-interpolable-fonts
         define-space
         use-glyphs
         )
   
         
(define fontmath-object/c
  (flat-named-contract 
   'fontmath-object/c
   (or/c vec? real? ffont? fglyph? bezier/c)))

(define font-object/c
  (flat-named-contract 
   'font-object/c
   (or/c vec? ffont? fglyph? fcontour? fanchor/c fcomponent/c finfo/c fkerning/c)))

(define use-only
  (make-parameter #f))

(define-syntax use-glyphs 
  (syntax-rules ()
    [(interpolate-glyphs gs . body)
     (parameterize [(use-only gs)] . body)]))

; FFont (listof Symbol) -> FFont
(define (only-glyphs-in gl f)
    (struct-copy ffont f
                 [glyphs (filter (lambda (g)
                                   (member (fglyph-name g) gl))
                                 (ffont-glyphs f))]))

; Symbol FFont -> (listof Symbol)
(define (component-deps g f)
    (let ([cs (map component-base (fglyph-components (car (fget-glyphs f (list g)))))])
      (append* cs (map (lambda (g) (component-deps g f)) cs))))

; FFont -> FFont
(define (reduced-font f)
  (let ([ls (if (use-only)
                 (remove-duplicates 
                  (apply append (use-only) 
                         (map (lambda (g) (component-deps g f)) (use-only))))
                 #f)])
    (if ls (only-glyphs-in ls f) f)))

; FContour ... -> FContour
(define (contour+ c1 . cs)
  (struct-copy fcontour c1
               [points (apply map 
                              (lambda (p1 . ps)
                                (foldl vec+ (vec 0 0) (cons p1 ps)))
                              (fcontour-points c1)
                              (map fcontour-points cs))]))

; Fcomponent ... -> Fcomponent
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

; FAnchor ... -> FAnchor
(define (anchor+ a1 . as)
  (struct-copy anchor a1
               [pos (foldl vec+ (get-position a1)
                           (map get-position as))]))

; FGlyph ... -> FGlyph
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


; FFont ... -> FFont
(define (font+ f1 . fs)
  (let ([fonts (map reduced-font (cons f1 fs))])
    (struct-copy ffont f1
                 [info (apply info+ (map ffont-info fonts))]
                 [kerning (apply kerning+ (map ffont-kerning fonts))]
                 [glyphs (apply map glyph+ (map ffont-glyphs fonts))])))

; FInfo ... -> FInfo
(define (info+ i1 . is)
  (letrec [(aux (lambda (i1 i2)
                  (dict-map i1
                            (lambda (key value)
                              (let [(v2 (dict-ref i2 key))]
                                (cons key
                                      (match value
                                        [(list _ ...) (map + value v2)]
                                        [(? real? value) (+ value v2)]
                                        [_ (error "cannot add info")])))))))]
    (foldl aux i1 is)))

; FKerning ... -> FKerning
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


; FFont Real ... -> FFont
(define (font* f s1 . ss)
  (scale (reduced-font f) (apply * (cons s1 ss))))

; FontObject Real ... -> FontObject
(define (font:* o s1 . ss)
  ((match o
    [(ffont _ _ _ _) font*]
    [(fglyph _ _ _ _ _) scale]
    [(contour _ _) scale]
    [(anchor _ _ _ _) scale]
    [(component _ _ _) scale])
   o
   (apply * (cons s1 ss))))

; FontObject ... -> FontObject
(define (font:+ o1 . os)
  (apply (match o1
           [(ffont _ _ _ _) font+]
           [(fglyph _ _ _ _ _) glyph+]
           [(contour _ _) contour+]
           [(anchor _ _ _ _) anchor+]
           [(component _ _ _) component+])
         (cons o1 os)))

; FontObject ... -> FontObject
(define (font:- o1 . os)
  (apply font:+ (cons o1 (map (lambda (o) (font:* o -1))
                                  os))))

; FontObject Real ... -> FontObject
(define (font:/ o s1 . ss)
  (font:* o (apply * (map (lambda (s) 
                                (/ 1 s)) 
                              (cons s1 ss)))))

; FontMathObject ... -> FontMathObject
(define (add a . as)
  (match (cons a as)
    [(list (? ffont? _) ...)
     (apply font:+ a as)]
    [(list (? fglyph? _) ...)
     (apply font:+ a as)]
    [(list (? real? _) ...)
     (apply + a as)]
    [(list (? vec? _) ...)
     (foldl vec+ a as)]
    [(list (? bezier/c _) ...)
     (foldl (lambda (a b) (map vec+ a b)) a as)]
    [_ (error "Invalid operands for product for addition")]))

; FontMathObject -> FontMathObject
(define (sub a . as)
  (match (cons a as)
    [(list (? ffont? _) ...)
     (apply font:+ a (map (lambda (i) (prod i -1)) as))]
    [(list (? fglyph? _) ...)
     (apply font:+ a (map (lambda (i) (prod i -1)) as))]
    [(list (? real? _) ...)
     (apply - a as)]
    [(list (? vec? _) ...)
     (foldl vec- a as)]
    [(list (? bezier/c _) ...)
     (foldl (lambda (a b) (map vec- a b)) a as)]
    [_ (error "Invalid operands for product for addition")]))

; FontMathObject ... -> FontMathObject
(define (prod a . as)
  (match (cons a as)
    [(list-no-order (? ffont? f) (? real? s) ...)
     (apply font:* f s)]
    [(list-no-order (? fglyph? f) (? real? s) ...)
     (apply font:* f s)]
    [(list-no-order (? vec? v) (? real? s) ...)
     (vec* v (apply * s))]
    [(list-no-order (? bezier/c b) (? real? s) ...)
     (let ([f (apply * s)])
       (map (lambda (v) (vec* v f)) b ))]
    [(list (? real? x) ...)
     (apply * x)]
    [_ (error "Invalid operands for product")]))

; FontMathObject Real ... -> FontMathObject
(define (div a . as)
  (match (cons a as)
    [(list (? ffont? f) (? real? s) ...)
     (apply font:* f (map (lambda (n) (/ 1.0 n)) s))]
    [(list (? fglyph? f) (? real? s) ...)
     (apply font:* f (map (lambda (n) (/ 1.0 n)) s))]
    [(list (? vec? v) (? real? s) ...)
     (vec* v (apply * (map (lambda (n) (/ 1.0 n)) s)))]
    [(list (? bezier/c b) (? real? s) ...)
     (let ([f (apply * (map (lambda (n) (/ 1.0 n)) s))])
       (map (lambda (v) (vec* v f)) b ))]
    [(list (? real? x) ...)
     (apply / x)]
    [_ (error "Invalid operands for product")]))


;; PROJECTIONS

; FontObject -> FontObject
; project the object on the x axis (set every y coord. to zero)
(define (x-> o)
  (scale o 1 0))

; FontObject -> FontObject
; project the object on the y axis (set every x coord. to zero)
(define (y-> o)
  (scale o 0 1))  

; FFont ... -> (listof FFont)
(define (interpolables f . fs)
  (let ([f0 (foldl (lambda (f acc)
                     (let-values ([(a b) (compatible-fonts acc f)])
                       a))
                   f fs)])
    (cons f0 (map (lambda (f)
                    (let-values ([(a b) (compatible-fonts f f0)])
                      (match-fonts-contours f0 a)))
                  fs))))

(define-syntax-rule (define-interpolable-fonts (id ...) f ...)
  (define-values (id ...)
    (apply values (get-interpolable-fonts f ...))))

; Font ... -> FFont ...
(define (get-interpolable-fonts . fs)
  (apply interpolables
         (map (lambda (u)
                (prepare-for-interpolation
                 (ufo->ffont u)
                 #f))
              fs)))

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
                                                      

