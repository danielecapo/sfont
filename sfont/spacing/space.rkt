#lang racket
(require "../main.rkt"
         "../geometry.rkt"
         (for-syntax racket/syntax))

(provide
 (contract-out 
  [spacer/c (-> any/c boolean?)]
  [get-spacing (-> font? (listof spacer/c) (listof (list/c name/c real? real?)))]
  [lowercase-tracy (->* (font? real? real? real? real?) (real? real?) font?)]
  [uppercase-tracy (-> font? real? real? real? real? font?)])
 space 
 kern
 space-glyph
 add-kern
 ;space-glyphs
 )
 
; Spacer
; (list Symbol (Number or False) (Number or False))

(define spacer/c 
  (flat-named-contract 
   'spacer/c 
   (list/c name/c (or/c real? #f) (or/c real? #f))))

; Adjustment
; (list Symbol Number Number)

; Font (Listof Spacer) -> (listof (list Symbol Real Real))
; produces a list of sidebearings for given glyphs
(define (get-spacing f s)
  (map (lambda (s)
         (let* ([name (car s)]
                [g (get-glyph f name)]
                [sleft (cadr s)]
                [sright (caddr s)])
           (cons name
                 (cons (if sleft
                           (car (get-sidebearings-at g f sleft))
                           (car (get-sidebearings g f)))
                       (if sright
                           (cdr (get-sidebearings-at g f sright))
                           (cdr (get-sidebearings g f)))))))
       s))
                       
                         
; spacing
; Font (Listof spacer) -> Font
; REMOVE?
(define (spacing f s)
  (define (set-space s f)
    (let* ([g (get-glyph f (car s))]
           [sleft (cadr s)]
           [sright (caddr s)]
           [sb (if (or (list? sleft)
                       (list? sright))
                   (get-sidebearings g f)
                   #f)]
           [nleft (if (number? sleft)
                      sleft;(- sleft (car (sidebearings f name)))
                      (+ (car sb) 
                         (- (car sleft) (car (get-sidebearings-at g f (cadr sleft))))))]
           [nright (if (number? sright)
                       sright ;(- sright (cdr (sidebearings f name)))
                       (+ (cdr sb)
                          (- (car sright) (cdr (get-sidebearings-at g f (cadr sright))))))])
      (insert-glyph f (set-sidebearings g f nleft nright))))
  (foldl set-space f s))

; space-macro
(define-syntax space
  (syntax-rules (groups)
    [(space f 
            [groups (name glyphs) ...]
            . spacing-forms)
     (let ([name glyphs] ...)
       (space-glyphs f . spacing-forms))]
    [(space f . spacing-forms)
     (space f [groups] . spacing-forms)]))

(define-syntax space-glyphs
  (syntax-rules (: @)
    [(space-glyphs f (name ...) : l r . spacing-forms)
     (let ([group (list 'name ...)])
       (space-glyphs f @ group : l r . spacing-forms))]
    [(space-glyphs f name : l r . spacing-forms)
     (let ([fo f])
       (space-glyphs (insert-glyph fo (space-glyph fo (get-glyph fo 'name) l r)) . spacing-forms))]
    [(space-glyphs f) f]
    [(space-glyphs f @ group : l r . spacing-forms)
     (let ([fo f])
       (space-glyphs 
        (foldl (lambda (g fo) 
                 (insert-glyph fo (space-glyph fo (get-glyph fo g) l r)))
               fo group)
        . spacing-forms))]))


(define-syntax space-glyph
  (syntax-rules (-- <-> /--/)
    [(space-glyph f g left right)
     (let ([gd (decompose-glyph f g)])
       (space-glyph gd left right))]
    [(space-glyph g -- --) g]
    [(space-glyph g (/--/ a) --)
     (struct-copy glyph g 
                  [advance (struct-copy advance (glyph-advance g) 
                                        [width a])])]
    [(space-glyph g l (/--/ a))
     (struct-copy glyph (space-glyph g l --) 
                  [advance (struct-copy advance (glyph-advance g) 
                                        [width a])])]
    [(space-glyph g (/--/ a) (<-> r))
     (let ([delta (- r (- a (advance-width (glyph-advance g))))])
       (space-glyph (struct-copy glyph g 
                                   [advance (struct-copy advance (glyph-advance g) 
                                                         [width a])])
                    (<-> (- delta)) (<-> delta)))]
    [(space-glyph g (/--/ a) r)
     (let* ([gt (space-glyph g -- r)]
            [l (car (get-sidebearings g))]
            [at (advance-width (glyph-advance gt))]
            [delta (- a at)])
       (space-glyph gt (<-> delta) --))]
    [(space-glyph g (<-> l) r)
     (space-glyph (adjust-sidebearings g l #f) -- r)]
    [(space-glyph g l (<-> r))
     (space-glyph (adjust-sidebearings g #f r) l --)]
    [(space-glyph g l (r mr))
     (space-glyph (set-sidebearings-at g #f r mr) l --)]
    [(space-glyph g (l ml) r)
     (space-glyph (set-sidebearings-at g l #f ml) -- r)]
    [(space-glyph g -- r)
     (set-sidebearings g #f r)]
    [(space-glyph g l --)
     (set-sidebearings g l #f)]
    [(space-glyph g l r)
     (space-glyph (space-glyph g l --)
                  -- r)]))


; (space-glyph g left-form right-form) ->
;   (let ([l ---]
;         [r ---])
;    (adjust-sidebearings g l r)
(define-syntax (sg stx)
  (syntax-case stx (/--/ --)
    [(_ g f -- (/--/ aw))
     #'(sg g -- (/--/ aw))]
    [(_ g -- (/--/ aw))
     #'(struct-copy glyph g
                    [advance (struct-copy advance (glyph-advance g)
                                          [width aw])])]
    [(_ g left-form right-form)
     #'(sg g #f left-form right-form)]
    [(_ g f left-form right-form)
     (with-syntax ([gd #'(if f 
                             (decompose-glyph f g)
                             g)])
       (let ([left (syntax-case #'left-form (-- <->)
                     [-- #'0]
                     [(<-> l) #'l]
                     [(l h) #'(- l (car (get-sidebearings-at gd h )))]
                     [l #'(- l (car (get-sidebearings gd)))])])
         (with-syntax ([l left])
           #'(let ([r right-form])
               (let ([fo f])
                 (if fo
                     (adjust-sidebearings g f l r)
                     (adjust-sidebearings g l r)))))))]))
  
(define fo (read-ufo "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_0.ufo"))
(define a (seq fo 'a))
; Symbol -> Symbol
; add public.kern1 to the group name
(define (left-kern-group n)
  (string->symbol (~a "public.kern1." n)))

; Symbol -> Symbol
; add public.kern2 to the group name
(define (right-kern-group n)
  (string->symbol (~a "public.kern2." n)))

(define-syntax kern
  (syntax-rules (left-groups right-groups)
    [(kern f 
           [left-groups  (ln lgs) ...]
           [right-groups (rn rgs) ...]
           . kern-forms)
     (let ([f1 f]
           [kh (make-hash)]
           [ln (left-kern-group 'ln)] ...
           [rn (right-kern-group 'rn)] ...)
       (struct-copy font f1
                    [groups (make-immutable-hash 
                             (list (cons ln lgs) ...
                                   (cons rn rgs) ...))]
                    [kerning (make-kerns f1 kh . kern-forms)]))]
    [(kern f [left-groups  (ln lgs) ...] . kern-forms)
     (kern f [left-groups  (ln lgs) ...] [right-groups] . kern-forms)]
    [(kern f [right-groups  (ln lgs) ...] . kern-forms)
     (kern f [left-groups] [right-groups  (ln lgs) ...] . kern-forms)]
    [(kern f . kern-forms)
     (kern f [left-groups] [right-groups] . kern-forms)]))

(define-syntax make-kerns
  (syntax-rules (: @)
    [(make-kerns f1 #f . kern-forms)
     (let ([k (make-hash)])
       (make-kerns f1 k . kern-forms))]
    [(make-kerns f1 kh @ l r : v . kern-forms)
     (make-kerns f1 (add-kern f1 kh (left-kern-group 'l) 'r v) . kern-forms)] 
    [(make-kerns f1 kh l @ r : v . kern-forms)
     (make-kerns f1 (add-kern f1 kh 'l (right-kern-group 'r) v) . kern-forms)] 
    [(make-kerns f1 kh @ l @ r : v . kern-forms)
     (make-kerns f1 (add-kern f1 kh (left-kern-group 'l) (right-kern-group 'r) v) . kern-forms)] 
    [(make-kerns f1 kh l r : v . kern-forms)
     (make-kerns f1 (add-kern f1 kh 'l 'r v) . kern-forms)] 
    [(make-kerns f1 kh) (make-immutable-kerning kh)]))

    
(define-syntax add-kern
  (syntax-rules ()
    [(add-kern f k l r v)
       (begin
         (if (hash-has-key? k l)
             (hash-set! (hash-ref k l) r v)
             (hash-set! k l (make-hash (list (cons r v)))))    
         k)]))

(define (make-immutable-kerning k)
  (make-immutable-hash
   (hash-map k (lambda (k v)
                 (cons k (make-immutable-hash (hash->list v)))))))

; define-spacing-rule
(define-syntax define-spacing-rule 
  (syntax-rules ()
    [(define-spacing-rule name (variable ...) (binding ...) (group ...) rule ...)
       (define (name font variable ...)
           (let (binding ...)
             (space font
                    [groups group ...]
                    rule ...)))]
    [(define-spacing-rule name (variable ...) (binding ...) rule ...)
     (define-spacing-rule name (variable ...) (binding ...) () rule ...)]))


; Font Real Real Real Real [Real] [Real] -> Font
; produce a font by applying the method described in W. Tracy's Letters of Credit
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
  a : -- (b mid)
  b : (a mid) e
  c : e f
  d : e (a mid)
  e : e f
  f : -- --
  g : -- --
  h : (c mid) (b mid)
  i : (c mid) (a mid)
  j : (a mid) (a mid)
  k : (c mid) d
  l : (c mid) (a mid)
  m : (a mid) (b mid)
  n : (a mid) (b mid)
  o : e e
  p : (c mid) e
  q : e (a mid)
  r : (a mid) d
  s : -- --
  t : -- --
  u : (b mid) (b mid)
  v : d d
  w : d d
  x : d d
  y : (d xh) (d xh)
  z : -- --)
                         
(define-spacing-rule
  uppercase-tracy 
  [caps h o min]
  ([mid (/ caps 2)]
   [a h]
   [b (floor (* 0.9 h))]
   [c (floor (/ h 2))]
   [d min]
   [e o])
  A : d d
  B : (a mid) c
  C : e c
  D : (a mid) e
  E : (a mid) c
  F : (a mid) c
  G : e (b (/ mid 2.5))
  H : (a mid) (a mid)
  I : (a mid) (a mid)
  J : d (a mid)
  K : (a mid) d
  L : (a mid) d
  M : (b mid) (a mid)
  N : (b mid) (b mid)
  O : e e
  P : (a mid) e
  Q : (e mid) (e mid)
  R : (a mid) d
  S : -- --
  T : d d
  U : (a mid) (b mid)
  V : d d
  W : d d
  X : d d
  Y : d d
  Z : c c)


;;;;;;;;;;;
; The following procedure were part of a silly idea 
; of spacing by examples
; it should be developed, meanwhile I don't provide them


; Font (listOf Adjustment) -> Font
; adjust the spacing
(define (adjust-spacing f s)
  (define (set-space s f) 
      (insert-glyph f (adjust-sidebearings (get-glyph f (car s)) f (cadr s) (caddr s))))
  (foldl set-space f s))


; Font (listOf Sidebearings) (listOf Symbol) (listOf Symbol) Number -> font
; apply the spacing to the selected sides
(define (set-spacing-sides f sp left right v)
  (adjust-spacing f (append (map (lambda (g) (list g (- v (car (dict-ref sp g))) 0)) left)
                            (map (lambda (g) (list g 0 (- v (cdr (dict-ref sp g))))) right))))


; Font (listOf Sidebearings) (listOf Symbol) (listOf Symbol) Number Number Number -> (listOf Font)
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
      (parameterize ([TEXT text]
                     [SIZE size])
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


;(define fo (read-ufo "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_1.ufo"))
;fo
      
#;
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
         
