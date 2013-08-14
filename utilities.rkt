#lang racket

(provide n-groups
         num->int
         code+expr
         square
         double
         °
         pi/2
         pi/3
         pi/4
         pi/6
         2pi)
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


(define (num->int n)
  (inexact->exact (floor n)))

(define-syntax code+expr
  (syntax-rules ()
    [(code+expr expr)
     (begin 
       (display "The expression:")
       (newline)
       (display (quote expr))
       (newline)
       (display "evaluates to:")
       (newline)
       expr)]))

(define (square n) (* n n))

(define (double n) (* n 2))

(define (degree->rad angle)
  (* pi (/ angle 180.0)))


; Number -> Number
; convert from degree to radians
(define (° d)
  (* (/ d 180) pi))

(define pi/2 (/ pi 2))
(define pi/3 (/ pi 3))
(define pi/4 (/ pi 4))
(define pi/6 (/ pi 6))
(define 2pi (* 2 pi))