#lang racket

(require "../sfont/parametric/fontwriter.rkt"
         "../sfont/main.rkt"
         "../sfont/utilities.rkt")

(provide sq)

;An example of the parametric 'language'
;the font macro can be used in two ways:
;1. to produce a 'static' font
;2. to produce a procedure that can be called with keyword arguments,
;these arguments are the parameters of the font
;
;The example here shows the second form.
;The font is divided in four areas
;1. name and parameters
;2. alignments 
;3. variables
;4. glyphs



(define sq 
  (font. (squarefont  [x-height 500] [width 0.5] [weight 0.5]) 
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
          [desc* (/ (- x-height 1000) 2) 0 :font-descender]
          [asc* (- x-height (alg desc*)) 0 :font-ascender]
          [descender (+ (alg desc*) 10) -10]
          [ascender (- (alg asc*) 10) 10])
         (variables
          ;; Variables are defined here
          ;; their scope is the whole font
          [v-stem (* x-height weight 0.333)]
          [gw (+ (* 2/3 v-stem) (* 1000 width))]
          [h-stem (* v-stem 0.9)]
          [counter-width (- gw (* 2 v-stem))]
          [space (/ counter-width (+ 1 (* 1/200 counter-width)))]
          [x1 space]
          [y1 (alg base)]
          [ym (/ x-height 2)]
          [x2 (+ space gw (- v-stem))]
          [a-cnts (list (rect x1 y1 gw h-stem)
                       (rect x1 y1 v-stem ym)
                       (rect x1 (- ym (/ h-stem 2)) gw h-stem)
                       (rect x2 y1 v-stem x-height)
                       (rect x1 (- x-height h-stem) gw h-stem))]
          [xh-stem (rect 0 y1 v-stem x-height)]
          [asc-stem (rect 0 y1 v-stem (alg ascender))]
          [desc-stem (rect 0 (alg descender) v-stem (- x-height (alg descender)))]
          [hor-stem (rect 0 y1 gw h-stem)]
          [counter-o (reverse (rect (+ 0 v-stem) 
                                    (+ y1 h-stem) 
                                    (- gw (* 2 v-stem)) 
                                    (- x-height (* 2 h-stem))))]
          [o-cnts (list (rect 0 y1 gw x-height)
                        counter-o)])
         (glyphs
          ;; Glyphs follow the variables section.
          ;; We can also provide a list of glyphs here.
          (glyph. 'space
                  (metrics -- (/--/ (double space))))
          (glyph. 'a
                  ; every glyph has a name 
                  (metrics space space)
                  ; an advance form
                  [contours a-cnts]
                  ;inside the contours section we can insert contours 
                  ;or list of contours
                  )
          (glyph. 'b
                  (metrics space (/--/ (+ gw space space)))
                  [contours
                   asc-stem
                   o-cnts])
          (glyph. 'c
                  (locals [term ym])
                  ; local variables can be defined inside a glyph
                  (metrics space space)
                  [contours
                   xh-stem
                   (translate. hor-stem 0 (- x-height h-stem))
                   hor-stem
                   (rect (- gw v-stem) (- x-height term) v-stem term)])
          (glyph. 'd
                  (metrics space space)
                  [contours
                   o-cnts
                   (translate. asc-stem (- gw h-stem) 0)])
          (glyph. 'e
                  (metrics space (/--/ (+ gw space space)))
                  [contours
                   (map (lambda (c) (from ((+ space (/ gw 2)) (/ x-height 2))
                                          (rotate. c pi)))
                        a-cnts)])
          (glyph. 'f
                  (metrics (space (/ x-height 2)) 0)
                  [contours 
                   asc-stem
                   (rect 0 (- (alg ascender) h-stem) (* gw 2/3) h-stem)
                   (rect (- (* gw 1/6)) (- x-height h-stem) (* gw 2/3) h-stem)])
          (glyph. 'g
                  (metrics space space)
                  [contours 
                   o-cnts
                   (translate. desc-stem (- gw h-stem) 0)
                   (translate. hor-stem 0 (alg descender))])
          (glyph. 'h
                  (metrics space space)
                  [contours 
                   asc-stem
                   (translate. hor-stem 0 (- x-height h-stem))
                   (translate. xh-stem (- gw v-stem) 0)])
          (glyph. 'i
                  (metrics space space)
                  [contours 
                   xh-stem
                   (rect 0 (* x-height 1.2) v-stem h-stem)])
          (glyph. 'l
                  (metrics space space)
                  [contours 
                   asc-stem])
          (glyph. 'n
                  (metrics space space)
                  [contours 
                   xh-stem
                   (translate. hor-stem 0 (- x-height h-stem))
                   (translate. xh-stem (- gw v-stem) 0)])
          (glyph. 'm
                  [locals (counter-width (* (- gw (double v-stem)) 0.8))]
                  (metrics space space)                  
                  [contours 
                   xh-stem
                   (translate. (rect 0 0 
                                     (+ (double (+ counter-width v-stem)) v-stem) 
                                     h-stem) 
                               0 (- x-height h-stem))
                   (translate. xh-stem (+ counter-width v-stem) 0)
                   (translate. xh-stem (double (+ counter-width v-stem)) 0)])
          
          (glyph. 'o
                  (metrics space space)
                  [contours o-cnts])
          (glyph. 'p
                  (metrics space space)
                  [contours 
                   o-cnts
                   desc-stem])
          (glyph. 'q
                  (metrics space space)
                  [contours 
                   o-cnts
                   (translate. desc-stem (- gw h-stem) 0)])
          (glyph. 'r
                  (metrics space (/ space 2))
                  [contours
                   xh-stem
                   (translate.
                    (scale. hor-stem 3/5 1)
                    0 (- x-height h-stem))])
          (glyph. 's
                  [locals (half-xhs (scale. xh-stem 1 0.5))]
                  (metrics space space)
                  [contours
                   hor-stem
                   (translate. hor-stem 0 (/ (- x-height h-stem) 2))
                   (translate. hor-stem 0 (- x-height h-stem))
                   (translate. half-xhs (- gw v-stem) 0)
                   (translate. half-xhs 0 (/ x-height 2))])
          (glyph. 't
                  (metrics (space (/ x-height 2)) 
                           ((* space 1.8) (/ x-height 2)))
                  [contours
                   (rect (- (* gw 1/6)) (- x-height h-stem) (* gw 2/3) h-stem)
                   (rect 0 0 v-stem 
                         (+ x-height (/ (- (alg ascender)
                                           x-height)
                                        2)))])
          (glyph. 'u
                  (metrics space space)
                  [contours 
                   xh-stem
                   hor-stem
                   (translate. xh-stem (- gw v-stem) 0)])
          (glyph. 'v
                  [locals (half-xhs (scale. xh-stem 1 0.5))]
                  (metrics space space)
                  [contours
                   (translate. (rect 0 0 (- gw (double v-stem)) h-stem) v-stem 0)
                   (translate. half-xhs 0 (/ x-height 2))
                   (translate. half-xhs v-stem 0)
                   (translate. half-xhs (- gw v-stem) (/ x-height 2))
                   (translate. half-xhs (- gw (double v-stem)) 0)])
          (glyph. 'z
                  [locals (half-xhs (scale. xh-stem 1 0.5))]
                  (metrics space space)
                  [contours
                   hor-stem
                   (translate. hor-stem 0 (/ (- x-height h-stem) 2))
                   (translate. hor-stem 0 (- x-height h-stem))
                   (translate. half-xhs (- gw v-stem) (/ x-height 2))
                   half-xhs])
          )))
                   
(display-size 80)  
(display-text 
 (string->text "abcdefghilm
nopqrstuvz"))
 
(sq)