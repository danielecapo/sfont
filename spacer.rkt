#lang racket
(require "font.rkt"
         "vec.rkt"
         "fontpict.rkt")

; get-spacing
; font, list of spacers -> font
; produces a list of sidebearings for given glyphs

(define (get-spacing f s)
  (map (lambda (s)
         (let* ([name (car s)]
                [sleft (cadr s)]
                [sright (caddr s)])
           (cons name
                 (cons (if sleft
                           (car (sidebearings-at f name sleft))
                           (car (sidebearings f name)))
                       (if sright
                           (cdr (sidebearings-at f name sright))
                           (cdr (sidebearings f name)))))))
       s))
                       
                       


; spacing
; font, list of spacer -> font

(define (spacing f s)
  (define (set-space s f)
    (let* ([name (car s)]
           [sleft (cadr s)]
           [sright (caddr s)]
           [nleft (if (number? sleft)
                      (- sleft (car (sidebearings f name)))
                      (- (car sleft) (car (sidebearings-at f name (cadr sleft)))))]
           [nright (if (number? sright)
                       (- sright (cdr (sidebearings f name)))
                       (- (car sright) (cdr (sidebearings-at f name (cadr sright)))))])
      (insert-glyph f (adjust-sidebearings f name nleft nright))))
  (foldl set-space f s))

; adjust-spacing
; font, list of adjustment -> font

(define (adjust-spacing f s)
  (define (set-space s f) 
      (insert-glyph f (apply adjust-sidebearings f s)))
  (foldl set-space f s))

; set-spacing-sides
; font, list of sidebearings, list of GlyphNames, list of GlyphNames, Number -> font
; apply the spacing to the selected sides

(define (set-spacing-sides f sp left right v)
  (adjust-spacing f (append (map (lambda (g) (list g (- v (car (dict-ref sp g))) 0)) left)
                            (map (lambda (g) (list g 0 (- v (cdr (dict-ref sp g))))) right))))

; samples
; font, list of sidebearings, list of GlyphNames, list of GlyphNames, Number, Number, Number -> List of fonts
; produce a list of fonts with different spacing

(define (samples f sp left right min max n)
  
  (let* ([step (/ (- max min) (- n 1))]
         [steps (map (lambda (n) (floor (+ min (* n step))))
                     (range n))])
    (map (lambda (s) 
           (cons s
                 (set-spacing-sides f sp left right s)))
           steps)))

          
    
(define (sample-choice samples n history text size)
  (let ([random-samples (shuffle samples)])
    (begin
      (with-sample-text [text size]
                        (for-each (lambda (s) 
                                    (print (cdr s))
                                    (newline)) 
                                  (take random-samples n)))
      (newline)
      (let ([c (read)])
        (cond [(= c 0) (floor (/ (foldl + 0 history) (length history)))]
              [(or (> c n) (< c 0)) (begin (display "invalid number")
                                           (newline)
                                           (sample-choice random-samples n history text size))]
              [else (sample-choice random-samples n
                                   (cons (car (list-ref random-samples (- c 1)))
                                         history) text size)])))))


(define (play-spacing f sp left right min max n n-show text size)
  (let* ([spc (get-spacing f sp)]
         [s (samples f spc left right min max n)]
         [r (sample-choice s n-show '() text size)])
    (begin
      (newline)
      (display r)
      (set-spacing-sides f spc left right r))))

(define (random-variations f left right v n)
  (map (lambda (x)
         (adjust-spacing f (append (map (lambda (g) (list g (- (random (* 2 v)) v) 0)) left)
                                   (map (lambda (g) (list g 0 (- (random (* 2 v)) v))) right))))
       
       (range n)))


(define-syntax (define-spacing-rule stx)
  (syntax-case stx ()
    [(define-spacing-rule name (variable ...) (binding ...) [(letter left right) ...])
     (with-syntax ([(sides ...) 
                    (map (lambda (l r)
                           (datum->syntax stx (let ([sl (syntax->datum l)]
                                                    [sr (syntax->datum r)])
                                                (list 'list 
                                                      (if (list? sl) (cons 'list sl) sl)
                                                      (if (list? sr) (cons 'list sr) sr)))))
                                                      
                         (syntax->list #'(left ...))
                         (syntax->list #'(right ...)))])
                           
       #'(define (name font variable ...)
           (let (binding ...)
             (spacing font (list (cons (quote letter) sides) ...)))))]))

; lowercase-tracy
; font, Number, Number, Number, Number -> font
; produces a font by applying te method described in W. Tracy's Letters of Credit

(define-spacing-rule
  lowercase-tracy 
  [xh n o min [c-adj 0.80] [l-adj 1.05]]
  ([mid (/ xh 2)]
   [a n]
   [b (floor (* 0.9 n))]
   [c (floor (* l-adj n))]
   [d min]
   [e o]
   [f (floor (* c-adj o))])
  [(a 0 (b mid))
   (b (a mid) e)
   (c e f)
   (d e (a mid))
   (e e f)
   (f 0 0)
   (g 0 0)
   (h (c mid) (b mid))
   (i (c mid) (a mid))
   (j (a mid) (a mid))
   (k (c mid) d)
   (l (c mid) (a mid))
   (m (a mid) (b mid))
   (n (a mid) (b mid))
   (o e e)
   (p (c mid) e)
   (q e (a mid))
   (r (a mid) d)
   (s 0 0)
   (t 0 0)
   (u (b mid) (b mid))
   (v d d)
   (w d d)
   (x d d)
   (y (d xh) (d xh))
   (z 0 0)])
                         
(define-spacing-rule
  uppercase-tracy 
  [asc h o min]
  ([mid (/ asc 2)]
   [a h]
   [b (floor (* 0.9 h))]
   [c (floor (/ h 2))]
   [d min]
   [e o])
  [(A d d)
   (B (a mid) c)
   (C e c)
   (D (a mid) e)
   (E (a mid) c)
   (F (a mid) c)
   (G e b)
   (H (a mid) (a mid))
   (I (a mid) (a mid))
   (J d (a mid))
   (K (a mid) d)
   (L (a mid) d)
   (M (b mid) (a mid))
   (N (b mid) (b mid))
   (O e e)
   (P (a mid) e)
   (Q (e mid) (e mid))
   (R (a mid) d)
   (S 0 0)
   (T d d)
   (U (a mid) (b mid))
   (V d d)
   (W d d)
   (X d d)
   (Y d d)
   (Z c c)])
#;
(define (lowercase-tracy font xh n o min [c-adj 0.80] [l-adj 1.05])
  (let ([mid (/ xh 2)]
        [a n]
        [b (floor (* 0.9 n))]
        [c (floor (* l-adj n))]
        [d min]
        [e o]
        [f (floor (* c-adj o))])
    (spacing
     font
     `((a 0 (,b ,mid))
       (b (,a ,mid) ,e)
       (c ,e ,f)
       (d ,e (,a ,mid))
       (e ,e ,f)
       (f 0 0)
       (g 0 0)
       (h (,c ,mid) (,b ,mid))
       (i (,c ,mid) (,a ,mid))
       (j (,a ,mid) (,a ,mid))
       (k (,c ,mid) ,d)
       (l (,c ,mid) (,a ,mid))
       (m (,a ,mid) (,b ,mid))
       (n (,a ,mid) (,b ,mid))
       (o ,e ,e)
       (p (,c ,mid) (,e ,mid))
       (q (,e ,mid) (,a ,mid))
       (r (,a ,mid) ,d)
       (s 0 0)
       (t 0 0)
       (u (,b ,mid) (,b ,mid))
       (v ,d ,d)
       (w ,d ,d)
       (x ,d ,d)
       (y (,d ,xh) (,d ,xh))
       (z 0 0)))))
      


(define fo (read-ufo "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_1.ufo"))
      

(define sp
    '((a #f #f)
      (b #f #f)
      (c #f #f)
      (d #f #f)
      (e #f #f)
      (f 230 230)
      (g #f #f)
      (h #f #f)
      (i 230 230)
      (j 230 230)
      (k #f #f)
      (l 230 230)
      (m #f #f)
      (n #f #f)
      (o #f #f)
      (p #f #f)
      (q #f #f)
      (r #f #f)
      (s #f #f)
      (t 230 #f)
      (u #f #f)
      (v #f #f)
      (w #f #f)
      (x #f #f)
      (y #f #f)
      (z #f #f)))
         