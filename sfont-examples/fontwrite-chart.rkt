#lang racket

(require "../sfont/parametric/fontwriter.rkt"
         "../sfont/utilities.rkt"
         "../sfont/main.rkt")

(provide c)

(define (slice cx cy radius angle)
  (if (= angle 0)
      '#f
      (~ (insert (arc cx cy radius angle)) (cx cy) -- ((+ cx radius) cy))))

(define (chart-slicer cx cy radius un)
  (lambda (pos ampl)
    (rotate (slice cx cy radius (* ampl un))
            from (cx cy)
            (* pos un))))

(define chartme
  (font. (chart [pos 0] [angle 1])
        (alignments
         [base 0 -10]
         [xh 550 10]
         [asc 775 0 :font-ascender]
         [desc -225 0 :font-descender])
        (variables
         [mid-v (/ (alg xh) 2)]
         [width (+ (alg xh) 100)]
         [mid-h (/ width 2)]
         [radius (+ mid-v (ovs-height xh) (abs (ovs-height base)))]
         [slicer (chart-slicer mid-h mid-v radius (/ pi 5))])
        [glyphs
         (glyph. 'a
                [metrics -- (/--/ width)]
                [contours
                 (slicer pos angle)])]))


(define-syntax chart-font
  (syntax-rules (->)
    [(chart-font divider (g -> ampl) ...)
     (font. chart
           (alignments
            [base 0 -10]
            [xh 550 10]
            [asc 775 0 :font-ascender]
            [desc -225 0 :font-descender])
           (variables
            [mid-v (/ (alg xh) 2)]
            [width (+ (alg xh) 100)]
            [mid-h (/ width 2)]
            [radius (+ mid-v (ovs-height xh) (abs (ovs-height base)))]
            [slicer (chart-slicer mid-h mid-v radius (/ (* 2 pi) divider))])
           [glyphs
            (glyph. 'space
                   [metrics -- (/--/ width)])
            (glyph. g
                   [metrics -- (/--/ 0)]
                   [contours
                    (slicer (foldl + 0 (range ampl)) ampl)]) ...])]))
 

(define c
  (chart-font
 10
 ('one -> 1)
 ('two -> 2)
 ('three -> 3)
 ('four -> 4)
 ('five -> 5)
 ('six -> 6)
 ('seven -> 7)
 ('eight -> 8)
 ('nine -> 9)
 ('zero -> 10))
)

(parameterize ([display-text (string->text "1 2 3 4 5 6 7 8 9 0")])
  (print c))


;(chartme)
;(chartme #:pos 1)
;(chartme #:pos 2)
;(chartme #:pos 3)
;(chartme #:pos 4)
;(chartme #:pos 5)
;(chartme #:pos 5)
;(chartme #:pos 6)
;(chartme #:pos 7)
;(chartme #:pos 8)
;(chartme #:pos 9)
;
;(chartme #:angle 1)
;(chartme #:angle 2)
;(chartme #:angle 3)
;(chartme #:angle 4)
;(chartme #:angle 5)
;(chartme #:angle 5)
;(chartme #:angle 6)
;(chartme #:angle 7)
;(chartme #:angle 8)
;(chartme #:angle 9)
;(chartme #:angle 10)

