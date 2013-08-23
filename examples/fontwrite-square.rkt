#lang racket

(require "../fontwriter.rkt")

;An example of the fontwriter 'language'
;the font macro can be used in two ways:
;1. to produce a 'static' font
;2. to produce a procedure that can be called with keyword arguments,
;these arguments are the parameters of the font
;
;The example here show the second form.
;The font is divided in four areas
;1. name and parameters
;2. alignments 
;3. variables
;4. glyphs


(define sq 
  (font (squarefont  [x-height 500] [width 0.5] [weight 0.5]) 
        ;; here we define a font named squarefont with parameters
        ;; x-height, width and weight
        ;; the parameters need a default value
      (alignments
       ;; the alignments are written in the form
       ;; [name value amount-of-overshoots].
       ;; It is required to mark with :use-as-descender and :use-as-ascender
       ;; two alignments that will be used as the font ascender and descender fields
       ;; in fontinfo: notice that the (+ ascender (abs descender)) is the UPM value
       ;; so if you want UPM to be 1000 you have to provide the right values.
       ;; Aligments are used with functions alg, ovs and ovs-height
       ;; for example (with parameter x-height set to 500)
       ;; (alg xh)        -> 500
       ;; (ovs xh)        -> 510
       ;; (ovs-height xh) ->  10
       
         [base 0 -10]
         [xh x-height 10]
         [desc* (/ (- x-height 1000) 2) 0 :use-as-descender]
         [asc* (- x-height (alg desc*)) 0 :use-as-ascender]
         [dsc (+ (alg desc*) 10) -10]
         [ascender (- (alg asc*) 10) 10])
      (variables
       ;; Variables are defined here
       ;; their scope is the whole font
       
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
     (glyphs
      ;; Glyphs follow the variables section.
      ;; We can also provide a list of glyphs here.
        (glyph 'a
               ; every glyph has a name 
               (metrics space space)
               ; an advance form
               [contours a-cnt]
               ;inside the contours section we can insert contours 
               ;or list of contours
               )
        (glyph 'b
               (metrics space (/--/ (+ gw space space)))
               [contours
                (rect x1 y1 v-stem (alg ascender))
                (rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))])
        (glyph 'c
               (locals [term ym])
               ; local variables can be defined inside a glyph
               (metrics (/--/ (+ gw space space)) space)
               [contours
                (rect x1 y1 v-stem x-height)
                (rect x1 (- x-height h-stem) gw h-stem)
                (rect x1 y1 gw h-stem)
                (rect (+ x1 gw (- v-stem)) (- x-height term) v-stem term)])
        (glyph 'd
               (metrics -- (/--/ (+ gw space space)))
               [contours
                (rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))
                (rect (+ x1 gw (- v-stem)) y1 v-stem (alg ascender))])
        (glyph 'e
               (metrics space (/--/ (+ gw space space)))
               [contours
                (map (lambda (c) (from ((+ space (/ gw 2)) (/ x-height 2))
                                       (rotate c pi)))
                     a-cnt)])
        (glyph 'o
               (metrics space space)
               [contours
                (rect x1 y1 gw x-height)
                (reverse (rect (+ x1 v-stem) (+ y1 h-stem) 
                               (- gw (* 2 v-stem)) (- x-height (* 2 h-stem))))]))))
        
(sq)