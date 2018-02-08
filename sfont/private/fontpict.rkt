#lang racket


(require racket/draw
         slideshow/pict
         (prefix-in geom: "../geometry.rkt")
         "../utilities.rkt")

(provide 
 (contract-out
  [display-size (parameter/c natural-number/c)]
  [display-text (parameter/c (listof (listof symbol?)))]
  [display-pen (parameter/c (is-a?/c pen%))]
  [show-kerning? (parameter/c boolean?)])
 set-contour-view! 
 with-contour-view
 pictf:font
 pictf:glyph
 draw-font-dc
 draw-glyph-dc
 lines
 unique-letters)

;;; Global variables
(define display-pen (make-parameter (new pen% [style 'transparent])))


(define display-size (make-parameter 100))
(define display-text (make-parameter '((a b c d e f g h i j k l m n o p q r s t u v w x y z))))
(define show-kerning? (make-parameter #t))


(define (set-contour-view! b) 
  (if b 
      (display-pen (new pen% [color "red"]))
      (display-pen (new pen% [style 'transparent]))))

(define-syntax-rule (with-contour-view . body)
  (parameterize ([display-pen (new pen% [color "red"])])
    . body))

;;; Line is one of:
;;; - nil
;;; - (cons Symbol Line)

;;; Text
;;; Text is one of:
;;; - nil
;;; - (cons Line Text)
;;; The text to be displayed

; Text -> Boolean
; True if it is a multiline text
(define (multiline? t)
  (> (length t) 1))

; Text -> Natural
; produce the total number of lines in text
(define (lines t)
  (length t))

; Text -> Line
; produce a line with all the glyph used in text without duplicates
(define (unique-letters t)
  (remove-duplicates (flatten t)))


      
(define (name glyph) (car glyph))
(define (advance glyph) (cadr glyph))
(define (contours glyph) (cddr glyph))


(define (draw-contour path c)
  (define (aux pts)
    (match pts
      [(list-rest (list-rest (list 'move _ _) _) _)
       (for-each (lambda (c) (draw-contour path c)) pts)]
      [(list) (send path close)]
      [(list-rest (list 'move x y) rest)
       (begin 
         (send path move-to x y)
         (aux rest))]
      [(list-rest (list 'off x1 y1) (list 'off x2 y2) (list x3 y3) rest)
       (begin
         (send path curve-to x1 y1 x2 y2 x3 y3)
         (aux rest))]
      [(list-rest (list x y) rest)
       (begin 
         (send path line-to x y)
         (aux rest))]))
  (aux c))


(define (pictf:draw-glyph dc glyph [kv 0])
  (begin
    (send dc translate kv 0)
    (define path (new dc-path%))
    
    (for-each (lambda (c) (geom:bezier->path c path))
              (contours glyph))
    (send dc draw-path path 0 0 'winding)
    (send dc translate (advance glyph) 0)))
    
(define (calculate-length glyphs)
  (foldr + 0 (map advance glyphs)))

; DC Number Number Number (listOf DrawableGlyph) ((Symbol Symbol) -> Number) -> void
; draw the font in the drawing context     
(define (draw-font-dc dc ascender descender leading glyphs [kerning (lambda (p) 0)] [size (display-size)] [text (display-text)])
  (let ([f (/  size (+ ascender (- descender)))])
    (begin
      (send dc set-brush "black" 'solid)
      (send dc set-pen (display-pen))
      (send dc scale f (- f))
      (send dc translate 0 (- (/ (* size -0.5) f) ascender))                      
      (for-each (lambda (l) 
                  (begin
                    (pictf:draw-line dc l (- (/ (* size leading) f)) glyphs 
                                     (if (show-kerning?) kerning (lambda (p) 0)))
                    (let ([current-x (vector-ref (vector-ref (send dc get-transformation) 0) 4)])
                      (send dc translate (/ (- current-x) f) 0))))
                text))))

; Number Number (listOf DrawableGlyph) ((Symbol Symbol) -> Number) -> pict
; draw the current (display-text)
(define (pictf:font ascender descender glyphs [kerning (lambda (p) 0)])
   (let* ([leading 1.2]
          [n-lines (lines (display-text))] 
          [area-height (* (display-size) (+ 1 n-lines (* (- leading 1) (- n-lines 1))))])
     (dc
      (lambda (dc dx dy)
        (let ([old-brush (send dc get-brush)]
              [old-pen (send dc get-pen)]
              [old-transform (send dc get-transformation)])
          (begin0
            (draw-font-dc dc ascender descender leading glyphs kerning)
            (send dc set-brush old-brush)
            (send dc set-pen old-pen)
            (send dc set-transformation old-transform))))
      1300 area-height)))

; DC Line Number (listOf DrawableGlyph) ((Symbol Symbol) -> Number) -> void
; draw the line to the dc
(define (pictf:draw-line dc l leading glyphs [kerning (lambda (p) 0)])
  (let* ([glyphs-to-display (filter identity (map (lambda (g) (assoc g glyphs)) l))]
         [k (if (null? glyphs-to-display)
                '()
                (cons 0 (map kerning (n-groups (map name glyphs-to-display) 2))))])
    (begin
      ;(print (send dc get-transformation))
      (for-each (lambda (g kv) (pictf:draw-glyph dc g kv)) glyphs-to-display k)
      (send dc translate 0 leading))))

#; 
(define (pictf:font ascender descender . glyphs)
   (let* ([letters (display-text)]
          [f (/  (display-size) (+ ascender (- descender)))]     
          [glyphs-to-display (filter identity (map (lambda (g) (assoc g glyphs)) letters))])
     (dc
      (lambda (dc dx dy)
         (begin
           (send dc set-brush "black" 'solid)
           (send dc set-pen (display-pen))
           (send dc scale f (- f))
           (send dc translate 0 (- (/ (* (display-size) -0.5) f) ascender))                      
           (for-each (lambda (g) (pictf:draw-glyph dc g)) glyphs-to-display )))
      1300 (* (display-size) 2))))


(define (draw-glyph-dc dc g f x-min y-max)
  (begin
    (send dc set-brush "black" 'solid)
    (send dc set-pen (display-pen))
    (send dc scale f (- f))
    (send dc translate (- x-min) (- y-max))
    (pictf:draw-glyph dc g)))
  
; Glyph BoundingBox (Number or False) (Number or False) -> pict
; Draw the glyph
(define (pictf:glyph g bb [upm #f])
  (let* ([vbb (geom:vec- (cdr bb) (car bb))]
         [w (geom:vec-x vbb)]
         [h (geom:vec-y vbb)]
         [x-min (geom:vec-x (car bb))]
         [by-max (geom:vec-y (cdr bb))]
;         [y-max (if ascender
;                    (max by-max ascender)
;                    by-max)]
         [f (cond [upm (/ 400 upm)]
                  [(> h 0) (/ 400 h)]
                  [else 1])])
    (dc
     (lambda (dc dx dy) (draw-glyph-dc dc g f x-min by-max))
       (* f w) (* f h))))
      