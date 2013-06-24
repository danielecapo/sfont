#lang racket

(provide n-groups)
; n-groups
; List [Any], Natural -> List of List [Any]
; (n-groups '(a b c d e f) 2) -> '((a b) (b c) (c d) (d e) (e f))
; (n-groups '(a b c d e f g) 3) -> '((a b c) (c d e) (e f g))

(define (n-groups lst n)
  (if (null? (cdr lst)) 
      null
      (let-values ([(f rest) (split-at lst (- n 1))])
        (cons (append f (list (car rest)))
              (n-groups rest n)))))