#lang racket

(provide (all-defined-out))
 
; In this file I define keywords that I will use as literal-ids in macros
; (probably not a really good idea to use all this special keywords, I will try to make cleaner macros in a future version).
; I keep them in one place so they can be easily identified.

(define-syntax groups (syntax-rules ()))
(define-syntax @ (syntax-rules ()))
(define-syntax -- (syntax-rules ()))
(define-syntax <-> (syntax-rules ()))
(define-syntax /--/ (syntax-rules ()))
(define-syntax : (syntax-rules ()))
(define-syntax side1 (syntax-rules ()))
(define-syntax side2 (syntax-rules ()))

(define-syntax @° (syntax-rules ()))
(define-syntax insert (syntax-rules ()))
(define-syntax >< (syntax-rules ()))

(define-syntax cycle (syntax-rules ()))
(define-syntax metrics (syntax-rules ()))
(define-syntax contours (syntax-rules ()))
(define-syntax locals (syntax-rules ()))
(define-syntax alignments (syntax-rules ()))
(define-syntax variables (syntax-rules ()))
(define-syntax glyphs (syntax-rules ()))
(define-syntax :font-ascender (syntax-rules ()))
(define-syntax :font-descender (syntax-rules ()))

(define-syntax --> (syntax-rules ()))