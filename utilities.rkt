#lang racket

(require "glyphlist.rkt")

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
  [2pi real?]
  [string->text (-> string? (listof (listof symbol?)))]))


; n-groups
; List [Any], Natural -> List of List [Any]
; (n-groups '(a b c d e f) 2) -> '((a b) (b c) (c d) (d e) (e f))
; (n-groups '(a b c d e f g) 3) -> '((a b c) (c d e) (e f g))
(define (n-groups lst n)
  (if (or (null? lst) (null? (cdr lst)))
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

; String -> Line
; produce a line from a string
(define (string->line s)
  (letrec ([m-read 
            (lambda (loc acc)
              (cond [(null? loc) (reverse acc)]
                    [(eq? #\space (car loc)) (m-read (cdr loc) (cons 'space acc))]
                    [(eq? #\newline (car loc)) (m-read (cdr loc) (cons 'space acc))]
                    [(eq? #\/ (car loc)) (read-name (cdr loc) acc '())]
                    [else (m-read (cdr loc)
                                  (cons
                                   (let ([adobe-name (get-name-by-code (char->integer (car loc)))])
                                     (if adobe-name 
                                         adobe-name 
                                         (string->symbol (list->string (list (car loc))))))
                                   acc))]))]
           [read-name (lambda (loc acc n)
                        (cond [(null? loc)
                               (m-read loc
                                       (cons (string->symbol (list->string (reverse n)))
                                             acc))]
                          [(eq? #\space (car loc))
                               (m-read (cdr loc)
                                       (cons (string->symbol (list->string (reverse n)))
                                             acc))]
                              [else (read-name (cdr loc) acc (cons (car loc) n))]))])
           (m-read (string->list s) '())))

; String -> Text
; produce a text from a string
(define (string->text s)
  (map string->line (string-split s "\n")))