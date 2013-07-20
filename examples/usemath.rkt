#lang racket

(require "../math.rkt")

;To run the code in this page download the Source Sans Pro Fonts from
;https://github.com/adobe/source-sans-pro
;the UFOs we will use are under the RomanMM directory.

;The macro define-fonts is used to load font for interpolations
;
;(define-fonts (id ...) 
;  ("path/to/font.ufo"
;   ...))
;
;It binds id to the corresponding font, the fonts are read, 
;trasformed in the format used for interpolations, then 
;outlines are processed to minimize non compatible glyphs.
;Glyphs that can't be interpolated are then automatically discarded.
;Adjust the paths so that they point the files in your computer


(define-fonts (light bold) ("/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_0.ufo"
                            "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_1.ufo"))

light

bold

;In this page the expressions are inside the macro code+expr (defined in utilities.rkt)
;similar to the macro pict+code (http://docs.racket-lang.org/quick/)
;to make the output (possibly) esiear to read.


;Summing a font to itself is the equivalent of scaling by a factor of 2.
;The operation simply produce a font with every number multiplied by 2,
;the result is visually the same as the font before the operation
;(since you it scale the UPM too).
;However components will be scaled too, the effect will be the composition
;of scaling the base glyph and scaling the component itself

(code+expr 
 (+ bold bold)
 )

(code+expr
 (* bold 2)
 )

(code+expr
 (/ bold 1.5)
 )

;The following expression is equivalent two find the font between light and bold
;You can see that the expression is equivalent to the expression used for
;interpolating numbers or vectors.

(code+expr
 (+ light (* 0.5 (- bold light)))
 )

;The procedure x-> and y-> produce fonts with every point projected on the x and y axes.

;The following expression produce a font equal to bold (by recomposing the x and y components

(code+expr
 (+ (x-> bold) (y-> bold))
 )

;The following expression stretch the font


(code+expr
 (+ (* 3 (x-> bold)) (y-> bold))
 )

;If you want to interpolate only the x axis (increasing the weight of vertical stems
;but keeping the horinzontal stems), you can obtain an x-only version of your fonts
;with the procedure x-> (equivalent to projecting every point on the x axis).
;The procedure y-> is used for y axis

(code+expr
 (+ light (* 0.5 (- (x-> bold) (x-> light))))
 )

(code+expr
 (+ light (* 0.5 (- (y-> bold) (y-> light))))
 )

;If we sum (x-> light) to bold the result is quite interesting.
;It looks like a bold-wide version of the font.
;The reason is simple, we are summing only the x coordinates of light,
;therefore the weight of horizontal lines remain the same of bold,
;at the same time the weight of vertical lines increased a little bit
;since light is really light; the larger effects is seen on the counters
;because in our light font the counters are very large compared to stems,
;thus producing the effect of bold wide font.


(code+expr
 (+ bold (x-> light))
 )

;a better looking bold-wide can probably be achieved by multiplying
;(x-> light) by 0.3 or 0.4.

(code+expr
 (+ bold (* 0.4 (x-> light)))
 )

;Glyphs with components, however, show several problems
;because the scale fields in the component matrix are affected
;by multiplications and additions.
;We can use the utility function fix-components for better results
;It copies the scale fields in components from a 'good' font to the font we want to improve.

(code+expr
 (fix-components (+ bold (* 0.4 (x-> light)))
                 bold)
 )

;now we can define a variable for bold-wide


(define bold-wide (fix-components (+ bold (* 0.4 (x-> light)))
                                  bold))


;Now we have three fonts that can be arranged in two axes:
;bold---bold-wide
;and bold---light
;we have seen that the interpolation
;expression above looks like an operation on vectors.
;The following expression will produce a light-wide approximation

(code+expr
 (+ bold (+ (- bold-wide bold) (- light bold)))
 )

;or we can obtain every other nuance

(code+expr
 (+ bold (+ (* 0.6 (- bold-wide bold)) (* 0.2 (- light bold))))
 )

(code+expr
 (+ bold (+ (* -0.4 (- bold-wide bold)) (* 0.5 (- light bold))))
 )

;We can recognize a pattern.
;You can define a 'space', with a font playing the role of the origin
;to simplify the syntax

(define-space s1 (bold [light bold-wide]))


;define-space define a procedure s1 and the fonts s1-light and s1-bold-wide
;that are (- light bold) and (- bold-wide bold)
;the operations performed inside s1 are automatically converted in the form seen above

(code+expr
 (s1
  (+ s1-bold-wide s1-light))
 )

;is equivalent to (+ bold (+ (- bold-wide bold) (- light bold)))
;This notation looks cleaner:

(code+expr
 (s1 
  (+ (* 0.6 s1-bold-wide)
     (* 0.2 s1-light)))
 )
;we can still use x-> and y->

(code+expr
 (s1 
  (+ (* 0.4 s1-bold-wide)
     (x-> s1-light)))
 )

(code+expr
 (s1 
  (+ (* 0.4 s1-bold-wide)
     (y-> s1-light)))
 )

;with math.rkt you get also transformations (translation, rotation, ...)
;that operate on glyphs (not touching fontinfo, etc.) with the exception of scale
;To scale glyphs without scaling infos use glyphs-scale
;warning: these operations are slow

(code+expr
 (rotate bold (/ pi 12))
 )

(code+expr
 (skew-x bold (/ pi -12))
 )

(code+expr
 (skew-y bold (/ pi -12))
 )

(code+expr
 (reflect-x bold)
 )

(code+expr
 (reflect-y bold)
 )

(code+expr
 (translate bold (vec -200 0))
 )

(code+expr
 (glyphs-scale bold 2.2 2)
 )

;Once you have done you can write your fonts to ufo
;uncomment the following lines and fill the path with the correct 
;path on your computer.
;The default setting is to export to UFO2, you can pass
;the keyword argument #:format 3 to convert to UFO3 
;(or to avoid conversion to the UFO2 format).
;If you want to ensure that the coordinates of points, anchors, etc.
;are rounded to integers pass the keyword argument #:round-coord #t,
;the default value is #f.

(write-font (s1 
             (+ (* 0.6 s1-bold-wide)
                (* 0.2 s1-light)))
            "export.ufo"
            #:round-coord #t)



