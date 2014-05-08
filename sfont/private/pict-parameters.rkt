#lang racket/base

(require racket/draw
         racket/class
         racket/contract/base)

(provide
 (contract-out
  [display-size (parameter/c natural-number/c)]
  [display-text (parameter/c (listof (listof symbol?)))]
  [display-leading (parameter/c number?)]
  [display-pen (parameter/c (is-a?/c pen%))]
  [show-kerning? (parameter/c boolean?)]
  [text-width (parameter/c natural-number/c)]
  [glyph-height (parameter/c natural-number/c)]))
  ;[glyph-draw-text (parameter/c (-> (is-a?/c dc<%>) void?))]))

; The pen used to trace contours
(define display-pen (make-parameter (new pen% [style 'transparent])))

; Size of text
(define display-size (make-parameter 100))

; Text to print
(define display-text (make-parameter '((a b c d e f g h i j k l m n o p q r s t u v w x y z))))

; Leading for multiline texts
(define display-leading (make-parameter 1.2))

; If true adjust spacing with kerning
(define show-kerning? (make-parameter #t))

; The width of the text pict
(define text-width (make-parameter 1300))

; The height of a glyph
(define glyph-height (make-parameter 400))

; The function used to draw glyphs in a text (DrawingContext -> void)
; (define glyph-draw-text (make-parameter (lambda (dc) (void))))




