#lang racket
(require "interpol.rkt"
         "flatfont.rkt"
         (prefix-in ufo: "font.rkt")
         "vec.rkt"
         "fontpict.rkt"
         (planet wmfarr/plt-linalg:1:13/matrix))

(provide (except-out (all-from-out racket) + - * /)
         (rename-out [prod *]
                     [add +]
                     [sub -]
                     [div /])
         (all-from-out "interpol.rkt")
         (all-from-out "fontpict.rkt")
         define-fonts
         define-space
         write-font
         with-sample-text
         set-sample-size!
         set-sample-text!
         )


(define current-transformation 
  (make-parameter
   (list->matrix
    '((1 0 0)
      (0 1 0)
      (0 0 1)))))

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

(define-syntax-rule (rot exp angle)
  (let [(rad-angle (degree->rad angle))]
    (parameterize ([current-transformation 
                    (matrix-mul (rotation-matrix rad-angle)
                                (current-transformation))])
      (trans-eval exp))))


(define (write-font f path format)
  (ufo:write-ufo ((if (= format 2)
                     ufo:ufo3->ufo2
                     ufo:ufo2->ufo3)
                  (flatfont:font->ufo f))
                 path))
  