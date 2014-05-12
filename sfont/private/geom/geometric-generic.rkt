#lang racket/base

(require gls)

(provide 
 (all-defined-out))

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