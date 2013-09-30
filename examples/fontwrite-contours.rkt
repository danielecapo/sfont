#lang racket
(require "../sfont/parametric/fontwriter.rkt"
         "../sfont/utilities.rkt")

;Contours examples
;
;Contours can be written with the ~ macro,
;they are transformed in a cubic bezier curves.
;In this file the beziers are printed with the print-beziers
;procedures that takes any number of bezier curves
;(printed with the same convention of postscript fonts
;a clockwise contour inside a counterclockwise contour
;is printed white.

;The macro code+expr imported from utilities.rkt
;is used to show the expression.
;Utilities also defines the function ° used to convert degree in radians
;and common angles: pi/2 pi/3 pi/4 pi/6 2pi

;Points in the ~ macros are written in the form (x y)
;The first point is define a point on the curve,
;the second and third points are control points,
;the fourth is a curve point, etc ...

(code+expr
 (print-beziers 
  (~ (0 -500) 
     (300 -500) (500 -300) (500 0)
     (500 300) (300 500) (0 500)
     (-300 500) (-500 300) (-500 0)
     (-500 -300) (-300 -500) (0 -500))))

; To draw a line between two points we can use --
(code+expr
 (print-beziers 
  (~ (0 0) -- (500 0) -- (500 500) -- (0 500) -- (0 0))))

; A curve is closed when the first and last points
; are the same. The command cycle can be used to copy
; the first point at the end and close the curve: 
(code+expr
 (print-beziers 
  (~ (0 0) -- (500 0) -- (500 500) -- (0 500) -- cycle)))

; Point positions can be absolutes or relatives
; Relative points are preceded by the @ sign.
; The first point can't be relative.
; The previous example can be rewritten:
(code+expr
 (print-beziers 
  (~ (0 0) -- (@ 500 0) -- (@ 0 500) -- (@ -500 0) -- cycle)))

; There's another way to wite a bezier segment
; let A, B and C be three non aligned points,
; where A and C are the extrema. The positions
; of control points A' and C' can be expressed as a fraction
; of AB and CB.
; We can use the form (x y t1 t2) to define the point B
; where t1 and t2 are the values controlling the positions
; of A' and C'.
; The short form (x y t) is equivalent to (x y t t).
; With (x y 0) A'=A and C'=C (the curve becomes a line)
; With (x y 1) A'=B and C'=B

(code+expr
 (print-beziers
  (~ (0 -500) (500 -500 0.6) (500 0) (500 500 1) (0 500) 
     (-500 500 0) (-500 0) (-500 -500 0 1) cycle)))

; If we already have a contour we can 'insert' it inside a macro
; and it will joined with a line to the previous point.
; If the inserted contour starts where the last point is
; the zero-length line segment will be removed

(define p1 (~ (0 -500) 
              (300 -500) (500 -300) (500 0)))
(code+expr
 (print-beziers 
  (~ (500 0) -- (0 500) -- (-500 0) (insert p1))))

; The @ sign can be used for insert
; (in this way the parts can be defined starting from zero
; maximizing the reusability):

(define rel-p1 (~ (0 0)
                  (300 0) (500 200) (500 500)))

(code+expr
 (print-beziers 
  (~ (500 0) -- (0 500) -- (-500 0) -- (0 -500) (@ insert rel-p1))))

; The primitives rect and ellipse accept four arguments:
; the x and y coordinates of the lower left corner
; the width and the height

(code+expr
 (print-beziers
  (rect 0 0 400 800)))

(code+expr
 (print-beziers
  (ellipse 0 0 400 500)))

; The arc primitive define an open arc
; given the center, the radius and the amplitude,
; angles are expressed in radians,
; the function ° can be used to transform degrees in radians

(code+expr
 (print-beziers 
  (arc 0 0 800 (° 60))))

(code+expr
 (print-beziers 
  (~ (0 0) (insert (arc 0 0 800 (° 120))) cycle)))

; Using relative insertion it is easy to draw the same
; shape in another position changing only one point

(code+expr
 (print-beziers 
  (~ (-200 -200) (@ insert (arc 0 0 800 (° 120))) cycle)))


; Transformations

(code+expr
 (print-beziers
  (translate (rect 0 0 400 600) 300 0)))

(code+expr
 (print-beziers
  (rotate (rect 0 0 400 600) (° 30))))

(code+expr
 (print-beziers
  (skew-x (rect 0 0 400 600) (° 30))))

(code+expr
 (print-beziers
  (skew-y (rect 0 0 400 600) (° 30))))

(code+expr
 (print-beziers
  (reflect-x (arc 0 0 500 (° 60)))))

(code+expr
 (print-beziers
  (reflect-y (arc 0 0 500 (° 60)))))


; transformations can be done relative to a point in two ways 

(code+expr
 (print-beziers
  (rect 0 0 400 600)
  (from (0 0) (rotate (rect 0 0 400 600) (° 90)))))



(print-beziers
 (rect 0 0 400 600)
 (from (0 300) (rotate (rect 0 0 400 600) (° 90))))



(print-beziers
 (rect 0 0 400 600)
 (rotate (rect 0 0 400 600) from (200 0) (° 90)))

