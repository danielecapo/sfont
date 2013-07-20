#lang racket
(require "interpol.rkt"
         "flatfont.rkt"
         (prefix-in ufo: "font.rkt")
         "vec.rkt"
         "fontpict.rkt"
         "utilities.rkt"
         (planet wmfarr/plt-linalg:1:13/matrix))

(provide (except-out (all-from-out racket) + - * /)
         (rename-out [prod *]
                     [add +]
                     [sub -]
                     [div /]
                     [m-translate translate]
                     [m-rotate rotate]
                     [m-scale scale]
                     [m-skew-x skew-x]
                     [m-skew-y skew-y]
                     [m-reflect-x reflect-x]
                     [m-reflect-y reflect-y]
                     [flatfont:font->ufo flatfont->ufo])
         (all-from-out "fontpict.rkt")
         (except-out (all-from-out "vec.rkt")
                     translate
                     rotate
                     scale
                     skew-x
                     skew-y
                     reflect-x
                     reflect-y)
         x->
         y->
         glyphs-scale
         define-fonts
         define-space
         code+expr
         write-font
         degree->rad
         fix-components)


;(define current-transformation 
;  (make-parameter
;   (list->matrix
;    '((1 0 0)
;      (0 1 0)
;      (0 0 1)))))


(define (m-translate a v)
  (match a
    [(? font? _)
     (font:translate a v)]
    [(? vec? _)
     (translate a v)]
    [_ (error "Invalid operand for translate")]))

(define (m-rotate a angle)
  (match a
    [(? font? _)
     (font:rotate a angle)]
    [(? vec? _)
     (rotate a angle)]
    [_ (error "Invalid operand for rotate")]))

(define (m-scale a fx [fy fx])
  (match a
    [(? font? _)
     (font:scale a fx fy)]
    [(? vec? _)
     (scale a fx fy)]
    [_ (error "Invalid operand for rotate")]))

(define (m-skew-x a angle)
  (match a
    [(? font? _)
     (font:skew-x a angle)]
    [(? vec? _)
     (skew-x a angle)]
    [_ (error "Invalid operand for skew-x")]))

(define (m-skew-y a angle)
  (match a
    [(? font? _)
     (font:skew-y a angle)]
    [(? vec? _)
     (skew-y a angle)]
    [_ (error "Invalid operand for skew-y")]))

(define (m-reflect-x a)
  (match a
    [(? font? _)
     (font:reflect-x a)]
    [(? vec? _)
     (reflect-x a)]
    [_ (error "Invalid operand for reflect-x")]))

(define (m-reflect-y a)
  (match a
    [(? font? _)
     (font:reflect-y a)]
    [(? vec? _)
     (reflect-y a)]
    [_ (error "Invalid operand for reflect-y")]))


(define (add a . as)
  (match (cons a as)
    [(list (? font? _) ...)
     (apply font:+ a as)]
    [(list (? number? _) ...)
     (apply + a as)]
    [(list (? vec? _) ...)
     (foldl vec+ a as)]
    [_ (error "Invalid operands for product for addition")]))

(define (sub a . as)
  (match (cons a as)
    [(list (? font? _) ...)
     (apply font:+ a (map (lambda (i) (prod i -1)) as))]
    [(list (? number? _) ...)
     (apply - a as)]
    [(list (? vec? _) ...)
     (foldl vec+ a (map (lambda (i) (prod i -1)) as))]
    [_ (error "Invalid operands for product for addition")]))


(define (prod a . as)
  (match (cons a as)
    [(list-no-order (? font? f) (? number? s) ...)
     (apply font:* f s)]
    [(list-no-order (? vec? v) (? number? s) ...)
     (vec* v (apply * s))]
    [(list (? number? x) ...)
     (apply * x)]
    [_ (error "Invalid operands for product")]))

(define (div a . as)
  (match (cons a as)
    [(list-no-order (? font? f) (? number? s) ...)
     (apply font:* f (map (lambda (n) (/ 1.0 n)) s))]
    [(list-no-order (? vec? v) (? number? s) ...)
     (vec* v (apply * (map (lambda (n) (/ 1.0 n)) s)))]
    [(list (? number? x) ...)
     (apply / x)]
    [_ (error "Invalid operands for product")]))


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
                          (flatfont:ufo->font (ufo:read-ufo p))
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

(define (degree->rad angle)
  (* pi (/ angle 180.0)))

;(define-syntax-rule (rot exp angle)
;  (let [(rad-angle (degree->rad angle))]
;    (parameterize ([current-transformation 
;                    (matrix-mul (rotation-matrix rad-angle)
;                                (current-transformation))])
;      (trans-eval exp))))

; fix-components
; Font, Font -> Font
; Produce a new font with components scale fields imported from f2

(define (fix-components f1 f2)
  (struct-copy font f1
               [glyphs (map (lambda (g1 g2)
                              (struct-copy glyph g1
                                           [components (map import-component-scale
                                                            (glyph-components g1)
                                                            (glyph-components g2))]))
                            (font-glyphs f1)
                            (font-glyphs f2))]))
                                                      


(define (write-font f path #:round-coord [round-coord #f] #:format [format 2])
  (let ([rf (if round-coord 
                (with-precision (1) (ufo:font-round (flatfont:font->ufo f)))
                (flatfont:font->ufo f))])
    (ufo:write-ufo ((if (= format 2)
                        ufo:ufo3->ufo2
                        ufo:ufo2->ufo3)
                    rf)
                   path)))
  