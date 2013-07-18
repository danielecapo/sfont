#lang racket

(require "../fontwriter.rkt")

(define sq 
  (font (squarefont  [x-height 500] [width 0.5] [weight 0.5]) 
      (alignments
         [base 0 -10]
         [xh x-height 10]
         [desc* (/ (- x-height 1000) 2) 0 :use-as-descender]
         [asc* (- x-height (alg desc*)) 0 :use-as-ascender]
         [dsc (+ (alg desc*) 10) -10]
         [ascender (- (alg asc*) 10) 10])
      (variables
         [gw (* 1000 width)]
         [v-stem (* x-height weight 0.333)]
         [h-stem (* v-stem 0.9)]
         [space (/ (- gw (* 2 v-stem)) 2)]
         [x1 space]
         [y1 (alg base)]
         [ym (/ x-height 2)]
         [x2 (+ space gw (- v-stem))]
         [a-cnt (list (rect x1 y1 gw h-stem)
                      (rect x1 y1 v-stem ym)
                      (rect x1 (- ym (/ h-stem 2)) gw h-stem)
                      (rect x2 y1 v-stem x-height)
                      (rect x1 (- x-height h-stem) gw h-stem))])
        (glyph 'a
               ()
               (/--/ (+ gw space space))
               [contours a-cnt])
        (glyph 'b
               ()
               (/--/ (+ gw space space))
               [contours
                (rect x1 y1 v-stem (alg ascender))
                (rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))])
        (glyph 'c
               ([term ym])
               (/--/ (+ gw space space))
               [contours
                (rect x1 y1 v-stem x-height)
                (rect x1 (- x-height h-stem) gw h-stem)
                (rect x1 y1 gw h-stem)
                (rect (+ x1 gw (- v-stem)) (- x-height term) v-stem term)])
        (glyph 'd
               ()
               (/--/ (+ gw space space))
               [contours
                (rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))
                (rect (+ x1 gw (- v-stem)) y1 v-stem (alg ascender))])
        (glyph 'e
               ()
               (/--/ (+ gw space space))
               [contours
                (map (lambda (c) (from ((+ space (/ gw 2)) (/ x-height 2))
                                       (rotate c pi)))
                     a-cnt)])
        (glyph 'o
               ()
               (/--/ (+ gw space space))
               [contours
                (rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))])))
        
(sq)