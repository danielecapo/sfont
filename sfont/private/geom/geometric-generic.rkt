#lang racket/base

(require gls
         (for-syntax racket/base
                     syntax/parse))

(provide 
 (except-out
  (all-defined-out)
  define-transform))

;;; Generic interface for geometric transformations
#;
(define-generics geometric
  (transform geometric m)
  (translate geometric x y)
  (scale geometric fx [fy])
  (rotate geometric a)
  (skew-x geometric a)
  (skew-y geometric a)
  (reflect-x geometric)
  (reflect-y geometric))

(defgeneric transform)
(defgeneric translate)
(defgeneric scale)
(defgeneric rotate)
(defgeneric skew-x)
(defgeneric skew-y)
(defgeneric reflect-x)
(defgeneric reflect-y)

(define-syntax-rule (from (x y) (fn o . args))
  (translate (fn (translate o (- x) (- y)) . args)
              x y))

  
; Syntax for defining geometric transformations
(define-syntax-rule (define-transform name fn)
  (define-syntax name
    (syntax-rules (from)
      [(name o from (x y) . args)
       (from (x y) (name o . args))]
      [(name o . args)
       (fn o . args)])))


; Transformation macros
(define-transform translate. translate)
(define-transform rotate. rotate)
(define-transform scale. scale)
(define-transform skew-x. skew-x)
(define-transform skew-y. skew-y)
(define-transform reflect-x. reflect-x)
(define-transform reflect-y. reflect-y)