#lang slideshow


(require racket/draw
         "geometry.rkt"
         "utilities.rkt")

(provide *size*
         *text*
         set-sample-size!
         set-sample-text!
         set-contour-view!
         show-kerning!
         hide-kerning!
         string->text
         with-sample-text
         pictf:font
         pictf:glyph
         unique-letters)

;;; Global variables
(define *pen* (new pen% [style 'transparent]))


(define *size* 100)
(define *text* '((a b c d e f g h i j k l m n o p q r s t u v w x y z)))
(define *show-kerning* #t)

(define (set-sample-size! s) (set! *size* s))
(define (set-sample-text! t) (set! *text* t))
(define (show-kerning!) (set! *show-kerning* #t))
(define (hide-kerning!) (set! *show-kerning* #f))

(define (set-contour-view! b) 
  (if b 
      (set! *pen* (new pen% [color "red"]))
      (set! *pen* (new pen% [style 'transparent]))))

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

; String -> Line
; produce a line from a string
(define (string->line s)
  (letrec ([m-read 
            (lambda (loc acc)
              (cond [(null? loc) (reverse acc)]
                    [(eq? #\space (car loc)) (m-read (cdr loc) (cons 'space acc))]
                    [(eq? #\newline (car loc)) (m-read (cdr loc) (cons 'space acc))]
                    [(eq? #\/ (car loc)) (read-name (cdr loc) acc '())]
                    [else (m-read (cdr loc)
                                  (cons (string->symbol (list->string (list (car loc))))
                                        acc))]))]
           [read-name (lambda (loc acc n)
                        (cond [(null? loc)
                               (m-read loc
                                       (cons (string->symbol (list->string (reverse n)))
                                             acc))]
                          [(eq? #\space (car loc))
                               (m-read (cdr loc)
                                       (cons (string->symbol (list->string (reverse n)))
                                             acc))]
                              [else (read-name (cdr loc) acc (cons (car loc) n))]))])
           (m-read (string->list s) '())))

; String -> Text
; produce a text from a string
(define (string->text s)
  (map string->line (string-split s "\n")))
      
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


(define (pictf:draw-glyph dc glyph [kv 0])
  (begin
    (send dc translate kv 0)
    (define path (new dc-path%))
    
    (for-each (lambda (c) (bezier->path c path))
              (contours glyph))
    (send dc draw-path path 0 0 'winding)
    (send dc translate (advance glyph) 0)))
    
(define (calculate-length glyphs)
  (foldr + 0 (map advance glyphs)))

; DC Number Number Number (listOf DrawableGlyph) ((Symbol Symbol) -> Number) -> void
; draw the font in the drawing context     
(define (draw-font-dc dc ascender descender leading glyphs [kerning (lambda (p) 0)])
  (let ([f (/  *size* (+ ascender (- descender)))])
    (begin
      (send dc set-brush "black" 'solid)
      (send dc set-pen *pen*)
      (send dc scale f (- f))
      (send dc translate 0 (- (/ (* *size* -0.5) f) ascender))                      
      (for-each (lambda (l) 
                  (begin
                    (pictf:draw-line dc l (- (/ (* *size* leading) f)) glyphs 
                                     (if *show-kerning* kerning (lambda (p) 0)))
                    (let ([current-x (vector-ref (vector-ref (send dc get-transformation) 0) 4)])
                      (send dc translate (/ (- current-x) f) 0))))
                *text*))))

; Number Number (listOf DrawableGlyph) ((Symbol Symbol) -> Number) -> pict
; draw the current *text*
(define (pictf:font ascender descender glyphs [kerning (lambda (p) 0)])
   (let* ([leading 1.2]
          [n-lines (lines *text*)] 
          [area-height (* *size* (+ 1 n-lines (* (- leading 1) (- n-lines 1))))])
     (dc
      (lambda (dc dx dy)
        (draw-font-dc dc ascender descender leading glyphs kerning))
      1300 area-height)))

; DC Line Number (listOf DrawableGlyph) ((Symbol Symbol) -> Number) -> void
; draw the line to the dc
(define (pictf:draw-line dc l leading glyphs [kerning (lambda (p) 0)])
  (let* ([glyphs-to-display (filter identity (map (lambda (g) (assoc g glyphs)) l))]
         [k (cons 0 (map kerning (n-groups (map name glyphs-to-display) 2)))])
    (begin
      ;(print (send dc get-transformation))
      (for-each (lambda (g kv) (pictf:draw-glyph dc g kv)) glyphs-to-display k)
      (send dc translate 0 leading))))

#; 
(define (pictf:font ascender descender . glyphs)
   (let* ([letters *text*]
          [f (/  *size* (+ ascender (- descender)))]     
          [glyphs-to-display (filter identity (map (lambda (g) (assoc g glyphs)) letters))])
     (dc
      (lambda (dc dx dy)
         (begin
           (send dc set-brush "black" 'solid)
           (send dc set-pen *pen*)
           (send dc scale f (- f))
           (send dc translate 0 (- (/ (* *size* -0.5) f) ascender))                      
           (for-each (lambda (g) (pictf:draw-glyph dc g)) glyphs-to-display )))
      1300 (* *size* 2))))


(define (draw-glyph-dc dc g f x-min y-max)
  (begin
    (send dc set-brush "black" 'solid)
    (send dc set-pen *pen*)
    (send dc scale f (- f))
    (send dc translate (- x-min) (- y-max))
    (pictf:draw-glyph dc g)))
  
; Glyph BoundingBox (Number or False) (Number or False) -> pict
; Draw the glyph
(define (pictf:glyph g bb [ascender #f] [upm #f])
  (let* ([vbb (vec- (cdr bb) (car bb))]
         [w (vec-x vbb)]
         [h (vec-y vbb)]
         [x-min (vec-x (car bb))]
         [by-max (vec-y (cdr bb))]
         [y-max (if ascender
                    (max by-max ascender)
                    by-max)]
         [f (cond [upm (/ 400 upm)]
                  [(> h 0) (/ 400 h)]
                  [else 1])])
    (dc
     (lambda (dc dx dy) (draw-glyph-dc dc g f x-min y-max))
       (* f w) (* f (if upm upm h)))))
      