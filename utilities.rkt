#lang racket

(provide 
 code+expr
 (contract-out
  [n-groups (-> (listof any/c) natural-number/c (listof (listof any/c)))]
  [num->int (-> rational? integer?)]
  [square (-> number? number?)]
  [double (-> number? number?)]
  [° (-> real? real?)]
  [pi/2 real?]
  [pi/3 real?]
  [pi/4 real?]
  [pi/6 real?]
  [2pi real?]))


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

; Rational -> Integer
; convert a rational number to integer
(define (num->int n)
  (exact-round n))

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

; Number -> Number
(define (square n) (* n n))

; Number -> Number 
(define (double n) (* n 2))

; Real -> Real
; convert from degree to radians
(define (° d)
  (* (/ d 180) pi))

; common angles
(define pi/2 (/ pi 2))
(define pi/3 (/ pi 3))
(define pi/4 (/ pi 4))
(define pi/6 (/ pi 6))
(define 2pi (* 2 pi))