#lang racket
(require "../parametric/fontwriter.rkt"
         "../utilities.rkt"
         "../fontpict.rkt")

; inspired by fontastic http://code.andreaskoller.com/libraries/fontastic/
(define alphabet-lc '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

; Number Number -> Bezier
; produce a rectangle whose width is l and height is s
(define (line l s)
  (let ([-s/2 (/ s -2)])
    (~ (0 -s/2) -- (@ l 0) -- (@ 0 s) -- (@ (- l) 0) -- cycle)))

; Symbol Number Number Number Number -> Ufo:Glyph
; produce a glyph with n lines 
(define (eq-glyph name n s sp w sb)
  (glyph. name
   (metrics sb sb)
   [contours 
    (map (lambda (p)
           (translate (line w s) 0 (* p sp)))
         (range n))]))

(define equalizer
  (font. (equalizer [width 300] [sb 20] [s 5])
        (alignments
         [base 0 0]
         [ascender 750 0 :use-as-ascender]
         [descender -250 0 :use-as-descender])
        (variables
         [space 28])
        (glyphs
         (glyph. 'space
                (metrics -- (/--/ (+ width sb sb)))
                [contours #f])
         (map (lambda (name n)
                (eq-glyph name n s space width sb))
              alphabet-lc
              (range 1 (+ 1 (length alphabet-lc)))))))
