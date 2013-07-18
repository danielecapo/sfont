#lang slideshow
(require racket/draw)

(require "vec.rkt"
         "bezier.rkt")

(provide *size*
         *text*
         set-sample-size!
         set-sample-text!
         with-sample-text
         pictf:font)

(define *size* 100)
(define *text* '(a b c d e f g h j k l m n o p q r s t u v w x y z))

(define (set-sample-size! s) (set! *size* s))
(define (set-sample-text! t) (set! *text* t))

(define-syntax-rule (with-sample-text (text size) body)
  (let ([t *text*]
        [s *size*])
    (begin
      (set-sample-text! text)
      (set-sample-size! size)
      body
      (set-sample-text! t)
      (set-sample-size! s))))
    

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


(define (pictf:draw-glyph dc glyph)
  (begin
    
    (define path (new dc-path%))
    
    (for-each (lambda (c) (draw-contour path c))
              (contours glyph))
    (send dc draw-path path 0 0 'winding)
    (send dc translate (advance glyph) 0)))
    
(define (calculate-length glyphs)
  (foldr + 0 (map advance glyphs)))
      
(define (pictf:font ascender descender . glyphs)
   (let* ([letters *text*];(map string->symbol (string-split *text* "/"))]
          [f (/  *size* (+ ascender (- descender)))]     
          [glyphs-to-display (filter identity (map (lambda (g) (assoc g glyphs)) letters))])
     (dc
      (lambda (dc dx dy)
         (begin
           (send dc set-brush "black" 'solid)
           (send dc scale f (- f))
           (send dc translate 0 (- (/ (* *size* -0.5) f) ascender))                      
           (for-each (lambda (g) (pictf:draw-glyph dc g)) glyphs-to-display )))
      1300 (* *size* 2))))
      