#lang racket
(require "../sfont/parametric/fontwriter.rkt"
         "../sfont/utilities.rkt"
         "../sfont/main.rkt")

(provide wave-fnt
         mount-fnt)

(define alphabet-lc '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define-syntax random-font
  (syntax-rules (ascender descender width variables)
    [(random-font 
      (name params ...)
      [ascender asc descender desc width w]
      draw-fn)
     (font. (name params ...)
           (alignments
            [ascender asc 0 :use-as-ascender]
            [descender desc 0 :use-as-descender])
           (variables)
           (glyphs
            (map (lambda (n)
                   (glyph. n 
                          (metrics -- (/--/ w))
                          [contours
                           (draw-fn)]))
                 alphabet-lc)))]))

(define wave-fnt
  (random-font 
   (wave [err 1])
   [ascender 750 descender -250 width 700]
   (lambda () (let* ([s 540]
                     [s/3 (/ s 3)]
                     [rnd (lambda () (random (inexact->exact (round (* err s/3)))))]
                     [side (lambda () (~ (0 0) (s/3 (rnd)) ((* s/3 2) (rnd)) (s 0)))])
                (translate (~ (insert (side)) 
                              (@ insert (rotate (side) pi/2))
                              (@ insert (rotate (side) pi))
                              (@ insert (rotate (side) (* 3 pi/2))))
                              
                           80 0)))))

(define mount-fnt
  (random-font 
   (mount [r 1])
   [ascender 750 descender -250 width 700]
   (lambda () (let* ([s 540])
                (translate (~ (0 0) -- (s 0) -- ((- s (random r)) (- 750 (random r)))
                              -- ((random r) (- 750 (random r))) -- cycle)
                           80 0)))))
  
   
(wave-fnt)
(wave-fnt)

(mount-fnt #:r 100)
                          
         
         