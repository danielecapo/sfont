#lang racket
(require "../fontwriter.rkt")

;Contours examples
;
;Contours can be written with the ~ macro,
;they are transformed in a cubic bezier curves.
;In this file the beziers are printed with the print-beziers
;procedures that takes a list of bezier curves.
;
;Points in the ~ macros are written in the form (x y)
;The first point is define a point on the curve,
;the second and third points are control points,
;the fourth is a curve point, etc ...

(print-beziers 
 (list (~ (0 -100) 
          (60 -100) (100 -60) (100 0)
          (100 60) (60 100) (0 100)
          (-60 100) (-100 60) (-100 0)
          (-100 -60) (-60 -100) (0 -100))))

; To draw a line between two points we can use --

(print-beziers 
 (list (~ (0 0) -- (100 0) -- (100 100) -- (0 100) -- (0 0))))

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

(print-beziers
 (list (~ (0 -100) (100 -100 0.6) (100 0) (100 100 1) (0 100) 
          (-100 100 0) (-100 0) (-100 -100 0 1) (0 -100))))

; If we already have a contour we can 'insert' it inside a macro
; and it will joined with a line to the previous point.

(define p1 (~ (0 -100) 
              (60 -100) (100 -60) (100 0)))
(print-beziers 
 (list (~ (100 0) -- (0 100) -- (-100 0) (insert p1))))

; The primitives rect and ellipse accept four arguments:
; the x and y coordinates of the lower left corner
; the width and the height

(print-beziers
 (list (rect 0 0 100 200)))


(print-beziers
 (list (ellipse 0 0 100 120)))

; The arc primitive define an open arc
; given the center, the radius and the amplitude,
; angles are expressed in radians,
; the function ° can be used to transform degrees in radians

(print-beziers 
 (list (arc 0 0 100 (° 60))))

(print-beziers 
 (list (~ (insert (arc 0 0 100 (° 120))) (0 0) -- (100 0))))

; Transformations

(print-beziers
 (list (translate (rect 0 0 100 200) 100 0)))

(print-beziers
 (list (rotate (rect 0 0 100 200) (° 30))))

(print-beziers
 (list (skew-x (rect 0 0 100 200) (° 30))))

(print-beziers
 (list (skew-y (rect 0 0 100 200) (° 30))))

(print-beziers
 (list (reflect-x (arc 0 0 100 (° 60)))))

(print-beziers
 (list (reflect-y (arc 0 0 100 (° 60)))))


; transformations can be done relative to a point
; in two ways (notice, printing is relative to the bounding box, so I will display the contours as text)

(define (print-as-text b)
 (for-each
  (lambda (v) (begin
                (display "(")
                (display (vec-x v)) 
                (display " ")
                (display (vec-y v))
                (display ")")
                (newline)))
  b))

(print-as-text
 (from (0 0) (rotate (ellipse 0 0 100 200) (° 90))))

(newline)
(newline)

(print-as-text
 (from (100 200) (rotate (ellipse 0 0 100 200) (° 90))))

(newline)
(newline)

(print-as-text
 (rotate (ellipse 0 0 100 200) from (100 200) (° 90)))