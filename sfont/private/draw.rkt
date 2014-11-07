#lang racket/base

(require racket/draw
         racket/class
         racket/function
         slideshow/pict
         (prefix-in geom: "../geometry.rkt")
         "../utilities.rkt")

(provide
 draw-text-in-dc
 draw-line-in-dc)

; DrawingContext Number Number Number Number Number (hash (Symbol . (-> void))) ((Symbol Symbol) -> Number) -> void
; draw the text in the drawing context     
(define (draw-text-in-dc dc ascender descender size text leading glyphs [kerning (lambda (p) 0)])
  (let ([f (/ size (+ ascender (- descender)))])
    (begin
      (send dc scale f (- f))
      (send dc translate 0 (- (/ (* size -0.5) f) ascender))                      
      (for-each (lambda (l) 
                  (begin
                    (draw-line-in-dc dc l (- (/ (* size leading) f)) glyphs kerning)
                    (let ([current-x (vector-ref (vector-ref (send dc get-transformation) 0) 4)])
                      (send dc translate (/ (- current-x) f) 0))))
                text))))

; DrawingContext Line Number (hash (Symbol . (-> void))) ((Symbol Symbol) -> Number) -> void
; draw the line in the drawing context 
(define (draw-line-in-dc dc l leading glyphs [kerning (lambda (p) 0)])
  (let* ([glyphs-to-display (filter identity (map (lambda (g) (and (hash-ref glyphs g #f) g)) l))]
         [k (if (null? glyphs-to-display)
                '()
                (cons 0 (map kerning (n-groups glyphs-to-display 2))))])
    (begin
      (for-each (lambda (g kv) 
                  (begin
                    (send dc translate kv 0)
                    ((hash-ref glyphs g) dc))) 
                glyphs-to-display k)
      (send dc translate 0 leading))))
