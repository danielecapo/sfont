#lang racket

(require "../../main.rkt")
(provide add-to-groups)

; Font Symbol (listof Symbol) -> Font
(define (add-to-groups f g gs)
  (struct-copy font f 
               [groups (hash-set (font-groups f) g gs)]))