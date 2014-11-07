#lang racket/base

(require racket/contract/base
         racket/list
         racket/match
         racket/function
         "interpolables.rkt"
         "info-kern-math.rkt"
         "../../main.rkt"
         "../../geometry.rkt"
         "../../properties.rkt"
         "../../utilities.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (except-out (all-from-out racket/base) + - * /)
         (contract-out
          [fontmath-object/c (-> any/c boolean?)]
          [font-intp-object/c (-> any/c boolean?)]
          [interpolables (->* (font?) (#:weak? boolean? #:auto-directions? boolean? #:match-contours? #f) #:rest (listof font?) (listof font?))]
          [rename prod * (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [rename add  + (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [rename sub  - (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [rename div  / (->* (fontmath-object/c) () #:rest (listof fontmath-object/c) fontmath-object/c)]
          [x-> (-> any/c any/c)]
          [y-> (-> any/c any/c)]
          [fix-components (-> font? font? font?)])
         define-interpolable-fonts
         define-space)
   
         
(define fontmath-object/c
  (flat-named-contract 
   'fontmath-object/c
   (or/c vec? real? font? glyph? bezier/c)))

(define font-intp-object/c
  (flat-named-contract 
   'font-object/c
   (or/c vec? font? glyph? layer? contour? anchor? component? fontinfo/c kerning/c)))


(define-syntax-rule (define-operation (name o1 o2) body)
  (define name
    (case-lambda 
      [(o1) o1]
      [(o1 o2) body]
      [(o1 . os) (foldl name o1 os)])))


; Point ... -> Point
; produce a new point summing the position vectors of all points
; and keeping the other fields of the first point
(define-operation (point+ p1 p2)
  (struct-copy point p1 [pos (vec+ (point-pos p1) (point-pos p2))]))
  
; Contour ... -> Contour
(define-operation (contour+ c1 c2)
  (struct-copy contour c1 
               [points (map point+ 
                            (contour-points c1) 
                            (contour-points c2))]))  

; Component ... -> Component
(define-operation (component+ c1 c2)
  (let ([cc1 (get-matrix c1)]
        [cc2 (get-matrix c2)])
    (struct-copy component c1
                 [matrix (match cc1
                           [(trans-mat x xy yx y xo yo)
                            (match cc2
                              [(trans-mat x2 xy2 yx2 y2 xo2 yo2)
                               (trans-mat (+ x x2) (+ xy xy2) (+ yx yx2) (+ y y2) (+ xo xo2) (+ yo yo2))])])])))

  
; Anchor ... -> Anchor
(define-operation (anchor+ a1 a2)
  (struct-copy anchor a1 [pos (vec+ (anchor-pos a1) (anchor-pos a2))]))

; Advance ... -> Advance
(define-operation (advance+ a1 a2)
 (struct-copy advance a1 
              [width (+ (advance-width  a1) (advance-width  a2))]
              [height (+ (advance-height  a1) (advance-height  a2))]))

; Layer ... -> Layer
(define-operation (layer+ l1 l2)
  (struct-copy layer l1
               [contours (map contour+ (layer-contours l1) (layer-contours l2))]
               [components (map component+ (layer-components l1) (layer-components l2))]
               [anchors (map anchor+ (layer-anchors l1) (layer-anchors l2))]))
  

; Glyph ... -> Glyph
(define-operation (glyph+ g1 g2)
  (struct-copy glyph g1
               [advance (advance+ (glyph-advance g1) (glyph-advance g2))]
               [layers (list (layer+ (get-layer g1 foreground)
                                     (get-layer g2 foreground)))]))

; Font Real Real -> Any
(define (font-scale* o fx [fy fx])
  (let ([o1 (scale o fx fy)])
    (struct-copy font o1
                 [fontinfo (info-scale (font-fontinfo o1) fx fy)]
                 [kerning (kerning-scale (font-kerning o1) fx)])))

; Font ... -> Font
(define-operation (font+ f1 f2)
  (struct-copy font f1
               [fontinfo (info+ (font-fontinfo f1) (font-fontinfo f2))]
               [kerning (kerning+ (font-kerning f1) (font-kerning f2))]
               [glyphs (map glyph+ (font-glyphs-list f1) 
                            (font-glyphs-list f2))]))


; Font Real ... -> Font
(define (font* f s1 . ss)
  (let ([s (apply * (cons s1 ss))])
    (if (= 1 s) f
        (font-scale* f s))))

; FontObject Real ... -> FontObject
(define (font:* o s1 . ss)
  ((match o
     [(? font? _) font*]
     [(? glyph? _) scale]
     [(? layer? _) scale]
     [(? contour? _) scale]
     [(? anchor? _ ) scale]
     [(? component? _) scale]
     [_ (error "Error: wrong type for font:*")])
   o
   (apply * (cons s1 ss))))

; FontObject ... -> FontObject
(define (font:+ o1 . os)
  (apply (match o1
           [(? font? _) font+]
           [(? glyph? _) glyph+]
           [(? layer? _) layer+]
           [(? contour? _) contour+]
           [(? anchor? _ ) anchor+]
           [(? component? _) component+]
           [_ (error "Error: wrong type for font:+")])
         (cons o1 os)))

; FontObject ... -> FontObject
(define (font:- o1 . os)
  (font:+ o1 (font:* (apply font:+ os) -1)))
          

; FontObject Real ... -> FontObject
(define (font:/ o s1 . ss)
  (font:* o (apply * (map (lambda (s) 
                                (/ 1 s)) 
                              (cons s1 ss)))))

; FontMathObject ... -> FontMathObject
(define (add a . as)
  (match (cons a as)
    [(list (? font? _) ...)
     (apply font:+ a as)]
    [(list (? glyph? _) ...)
     (apply font:+ a as)]
    [(list (? layer? _) ...)
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
    [(list _) (prod a -1)]
    [(list (? font? _) ...)
     (apply font:+ a (map (lambda (i) (prod i -1)) as))]
    [(list (? glyph? _) ...)
     (apply font:+ a (map (lambda (i) (prod i -1)) as))]
    [(list (? layer? _) ...)
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
    [(list-no-order (? font? f) (? real? s) ...)
     (apply font:* f s)]
    [(list-no-order (? glyph? f) (? real? s) ...)
     (apply font:* f s)]
    [(list-no-order (? layer? f) (? real? s) ...)
     (apply font:* f s)]
    [(list-no-order (? vec? v) (? real? s) ...)
     (vec* v (apply * s))]
    [(list-no-order (? bezier/c b) (? real? s) ...)
     (let ([f (apply * s)])
       (map (lambda (v) (vec* v f)) b ))]
    [(list (? real? x) ...)
     (apply * x)]
    [(list (? vec? v) ...)
     (foldl (lambda (v1 v2)
              (let* ([c1 (make-rectangular (vec-x v1) (vec-y v1))]
                     [c2 (make-rectangular (vec-x v2) (vec-y v2))]
                     [c (* c1 c2)])
                (vec (real-part c) (imag-part c))))
            a as)]
    [(list (? bezier/c b) ...)
     (foldl (lambda (b1 b2) (map prod b1 b2)) a as)]
    [_ (error "Invalid operands for product")]))

; FontMathObject Real ... -> FontMathObject
(define (div a . as)
  (match (cons a as)
    [(list (? font? f) (? real? s) ...)
     (apply font:* f (map (lambda (n) (/ 1.0 n)) s))]
    [(list (? glyph? f) (? real? s) ...)
     (apply font:* f (map (lambda (n) (/ 1.0 n)) s))]
    [(list (? layer? f) (? real? s) ...)
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

; Any -> Any
; project the object on the x axis (set every y coord. to zero)
(define (x-> o)
  ((if (font? o) font-scale* scale) o 1 0))

; Any -> Any
; project the object on the y axis (set every x coord. to zero)
(define (y-> o)
  ((if (font? o) font-scale* scale) o 0 1))  

; Font ... -> (listof Font)
(define (interpolables  f #:weak? [weak? #f] #:auto-directions? [auto-directions? #t] #:match-contours? [match-contours? #t] . fs)
  (let* ([fonts (map (curryr prepare-font weak? auto-directions?) (cons f fs))]
         [f0 (foldl (lambda (f acc)
                     (let-values ([(a b) (compatible-fonts acc f)])
                       a))
                   (car fonts) 
                   (cdr fonts))])
    (cons f0 (map (lambda (f)
                    (let-values ([(a b) (compatible-fonts f f0)])
                      (if match-contours?
                          (match-fonts-contours f0 a)
                          a)))
                  (cdr fonts)))))






; Font, Font -> Font
; Produce a new font with components scale fields imported from f2
(define (fix-components f1 f2)
  (struct-copy font f1
               [glyphs
                (map 
                 (lambda (g1 g2)
                   (struct-copy glyph g1
                                [layers 
                                 (list 
                                  (struct-copy layer (get-layer g1 foreground)
                                               [components (map import-component-scale
                                                                (layer-components (get-layer g1 foreground))
                                                                (layer-components (get-layer g2 foreground)))]))]))
                 (font-glyphs-list f1)
                 (font-glyphs-list f2))]))
                                                      


;;; MACROS
(define-syntax (define-interpolable-fonts stx)
  (define-splicing-syntax-class prepolation-parameters
    #:description "Parameters for prepolation"
    (pattern (~seq k:keyword v:expr)))
  (syntax-parse stx
    [(_ prepolation:prepolation-parameters ... (name:id f:expr) ...+)
     #'(define-values (name ...)
         (apply values (keyword-apply interpolables (list prepolation.k ...) (list prepolation.v ...) (list f ...))))]))

(define-syntax (define-space stx)
  (syntax-case stx ()
    [(define-space id (origin [font ...]))
     (for-each (lambda (i)
                 (unless (identifier? i)
                   (raise-syntax-error #f "Not an identifier" stx i)))
               (append (list #'id #'origin) (syntax->list #'(font ...))))
     (with-syntax ([(fname ...)
                    (map (lambda (f)
                           (format-id stx "~a-~a" #'id f))
                         (syntax->list #'(font ...)))])
       #'(begin
           (define (id f . fs)
             (apply values (map (lambda (f) (add origin f)) (cons f fs))))
           (define fname (sub font origin)) ...))]))

;(define f (read-ufo "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_0.ufo"))
;(define f1 (read-ufo "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_1.ufo"))
;(define-int [light f] [bold f1])
; 