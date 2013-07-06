#lang racket

(require "font.rkt")

(provide selector)
;(define (sel . glyphs)
;  (lambda (f)
;    
;        (map (lambda (g)
;               (cons g
;                     (map-layers
;                      (lambda (l) (cons () (get-glyph f g (layer-name l))))
;                      f)))
;             glyphs)))

(define (selector f)
  (lambda (glyph . glyphs)
    (letrec 
        ([gl (lambda (g)
               (let ([ret (filter identity
                                  (map-layers 
                                   (lambda (l)
                                     (let* ([layer-name (layer-name l)]
                                            [glyph (get-glyph f g layer-name)])
                                       (if glyph
                                           (cons layer-name glyph)
                                           #f)))
                                   f))])
                 (if (null? ret) #f ret)))])
      (if (null? glyphs)
          (gl glyph)
          (let ([ret (map (lambda (g) (cons g (gl g))) (cons glyph glyphs))])
            (filter (lambda (g) (cdr g)) ret))))))
      
