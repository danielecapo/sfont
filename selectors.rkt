#lang racket

(require "font.rkt")

(provide selector)
;(define (sel . glyphs)
;  (lambda (f)
;    
;        (map (lambda (g)
;               (cons g
;                     (ufo:map-layers
;                      (lambda (l) (cons (ufo:layer-name l) (ufo:get-glyph f g (ufo:layer-name l))))
;                      f)))
;             glyphs)))

(define (selector f)
  (lambda (glyph . glyphs)
    (letrec 
        ([gl (lambda (g)
               (let ([ret (filter identity
                                  (ufo:map-layers 
                                   (lambda (l)
                                     (let* ([layer-name (ufo:layer-name l)]
                                            [glyph (ufo:get-glyph f g layer-name)])
                                       (if glyph
                                           (cons layer-name glyph)
                                           #f)))
                                   f))])
                 (if (null? ret) #f ret)))])
      (if (null? glyphs)
          (gl glyph)
          (let ([ret (map (lambda (g) (cons g (gl g))) (cons glyph glyphs))])
            (filter (lambda (g) (cdr g)) ret))))))
      
