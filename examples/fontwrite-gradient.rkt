#lang racket
(require "../fontwriter.rkt"
         "../utilities.rkt"
         "../fontpict.rkt"
         "../ufopfa.rkt"
         "../writepfa.rkt")


(define alphabet-lc '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

; Symbol Symbol -> Symbol
; produce a new name with point and suffix added
(define (add-suffix s n)
  (string->symbol (~a n "." s)))

; Symbol (listOf Symbol) -> (listOf Symbol)
; produce a new list of name adding the suffix
(define (add-suffix-lon s lon)
  (let ([f (lambda (n) (add-suffix s n))])
    (map f lon)))

; Number Number Number -> Bezier
; produce a circle with center x, y and radius r
(define (circle x y r)
    (ellipse (- x r) (- y r) (* 2 r) (* 2 r)))

; Number Number Number -> (listOf Number)
; poduce a list of n circle of radius r distributed on a length l
(define (draw-row l n r)
  (let ([s (/ l n)])
  (map (lambda (p) (circle (* s p) 0 r))
       (range (+ n 1)))))

; Number Number -> Bezier
; produce a rectangle whose width is l and height is s
(define (hline l s)
  (let ([-s/2 (/ s -2)])
    (~ (0 -s/2) -- (@ l 0) -- (@ 0 s) -- (@ (- l) 0) -- cycle)))

; Number Number -> Bezier
; produce a rectangle whose width is l and height is s
(define (vline l s)
  (let ([-s/2 (/ s -2)])
    (~ (-s/2 0) -- (@ s 0) -- (@ 0 l) -- (@ (- s) 0) -- cycle)))

; Number Number -> Bezier
; produce an inclined rectangle whose width is l and height is s
(define (dline l s a)
  (skew-x (vline l s) a))

; Number Number Number Number -> Bezier
; produce a ring with center x, y, radius r and thickness s
(define (ring x y r s)
  (list 
   (circle x y (+ r (/ s 2)))
   (reverse (circle x y (- r (/ s 2))))))


; Symbol Number Number -> Ufo:Glyph
; produce a square glyph with circles of radius r
(define (dots-glyph name n r)
  (let ([s (num->int (/ 1000 n))])
    (glyph name
           [metrics -- (/--/ 1000)]
           [contours 
            (foldl append '()
                   (map (lambda (p)
                          (if (even? p)
                              (map (lambda (o) (translate o  (/ s 2) (+ (* p s) (/ s 2) -250))) (draw-row (- 1000 s) (- n 1) r))
                              (map (lambda (o) (translate o  s (+ (* p s) (/ s 2) -250))) (draw-row (- 1000 s) (- n 1) r))))
                        (range n)))])))

; Symbol Number Number -> Ufo:Glyph
; produce a square glyph with vertical stripes 
(define (vstripes-glyph name n r)
  (let ([s (num->int (/ 1000 n))])
    (glyph name
           [metrics -- (/--/ 1000)]
           [contours 
            (map (lambda (p)
                   (translate (vline 1000 r) (+ (/ s 2) (* p s)) -250))
                 (range n))])))

; Symbol Number Number -> Ufo:Glyph
; produce a square glyph with diagonal stripes 
(define (dstripes-glyph name n r a)
  (let ([s (num->int (/ 1000 n))])
    (glyph name
           [metrics -- (/--/ 1000)]
           [contours 
            (map (lambda (p)
                   (translate (dline 1000 r a) (+ (/ s 2) (* p s)) -250))
                 (range n))])))

; Symbol Number Number -> Ufo:Glyph
; produce a square glyph with horizontal stripes 
(define (hstripes-glyph name n r)
  (let ([s (num->int (/ 1000 n))])
    (glyph name
           [metrics -- (/--/ 1000)]
           [contours 
            (map (lambda (p)
                   (translate (hline 1000 r) 0 (+ (/ s 2) (* p s) -250)))
                 (range n))])))

; Symbol Number Number -> Ufo:Glyph
; produce a square glyph with rings
(define (rings-glyph name n r)
  (let ([s (num->int (/ 1000 n))])
    (glyph name
           [metrics -- (/--/ 1000)]
           [contours 
            (foldl append '()
            (map (lambda (p)
                   (ring 500 250 (+ (/ s 2) (* p s)) r))
                 (range (/ n 2))))])))

(define gradients
  (font (gradients [n 20])
        (alignments
         [base 0 0]
         [ascender 750 0 :use-as-ascender]
         [descender -250 0 :use-as-descender])
        (variables)
        (glyphs
         (glyph 'space
                (metrics -- (/--/ 1000))
                [contours #f])
         (glyph '.notdef
                (metrics -- (/--/ 0))
                [contours #f])
         (map (lambda (name r)
                (dots-glyph name n (* 1.3 r)))
              alphabet-lc
              (range 10 (+ 10 (length alphabet-lc))))
         (map (lambda (name r)
                (vstripes-glyph name n (* 1.7 r)))
              (add-suffix-lon 'vstr alphabet-lc)
              (range 10 (+ 10 (length alphabet-lc))))
         (map (lambda (name r)
                (dstripes-glyph name n (* 1.7 r) (° -10)))
              (add-suffix-lon 'dstr alphabet-lc)
              (range 10 (+ 10 (length alphabet-lc))))
         (map (lambda (name r)
                (dstripes-glyph name n (* 1.7 r) (° 10)))
              (add-suffix-lon 'dstr1 alphabet-lc)
              (range 10 (+ 10 (length alphabet-lc))))
         (map (lambda (name r)
                (hstripes-glyph name n (* 1.7 r)))
              (add-suffix-lon 'hstr alphabet-lc)
              (range 10 (+ 10 (length alphabet-lc))))
         (map (lambda (name r)
                (rings-glyph name n (* 1.7 r)))
              (add-suffix-lon 'ring alphabet-lc)
              (range 10 (+ 10 (length alphabet-lc))))
         )))

(set-sample-size! 40)
(set-sample-text!
   (list alphabet-lc
         (add-suffix-lon 'vstr alphabet-lc)
         (add-suffix-lon 'hstr alphabet-lc)
         (add-suffix-lon 'dstr alphabet-lc)
         (add-suffix-lon 'dstr1 alphabet-lc)
         (add-suffix-lon 'ring alphabet-lc)))
(define sh (gradients #:n 8))
sh
(define sh1
    (struct-copy ufo:font sh [fontinfo (let ([i (ufo:font-fontinfo sh)])
                                         (hash-set* i 
                                                    'postscriptIsFixedPitch #t
                                                    'postscriptBlueValues '(-10 0 740 750)
                                                    'postscriptOtherBlues '(-250 -240)
                                                    'postscriptStemSnapH '(23 45 57)
                                                    'postscriptStemSnapV '(23 45 57)))]))
;(write-type1 (ufo->pfa sh1 (cons (vec -150 -250) (vec 1150 750))) "gradients.txt")

