#lang racket/base

(require racket/list
         racket/contract/base
         racket/contract/region)

(provide
 (contract-out
  [line/c (-> any/c boolean?)]
  [text/c (-> any/c boolean?)])
 multiline?
 lines
 unique-letters)

; Line
; A Line is a list of Symbol
(define line/c (flat-named-contract 'list/c (listof symbol?)))

; Text
; A Text is a list of Lines
(define text/c (flat-named-contract 'text/c (listof line/c)))
 
; Text -> Boolean
; True if it is a multiline text
(define/contract (multiline? t)
  (-> text/c boolean?)
  (> (length t) 1))

; Text -> Natural
; produce the total number of lines in text
(define/contract (lines t)
  (-> text/c natural-number/c)
  (length t))

; Text -> Line
; produce a line with all the glyph used in text without duplicates
(define/contract (unique-letters t)
  (-> text/c line/c)
  (remove-duplicates (flatten t)))
