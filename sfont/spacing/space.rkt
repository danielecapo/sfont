#lang racket
(require "../main.rkt"
         "../geometry.rkt"
         (for-syntax racket/syntax
                     racket/list))

(provide
 (contract-out 
  [spacer/c (-> any/c boolean?)]
  [get-spacing (-> font? (listof spacer/c) (listof (list/c name/c real? real?)))]
  [lowercase-tracy (->* (font? real? real? real? real?) (real? real?) font?)]
  [uppercase-tracy (-> font? real? real? real? real? font?)])
 space 
 kern
 space-glyph
 add-kern)
 
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

; space macro
; The error messages should be more helpful
(define-syntax (space stx)
  (syntax-case stx (groups : @)
    [(space) 
     (raise-syntax-error #f "Expected Font" stx)]
    [(space f) #'f]
    [(space f 
            [groups (name glyphs) ...]
            . spacing-forms)
     #'(let ([name glyphs] ...)
         (space f . spacing-forms))]
    [(space f name : l r . spacing-forms)
     (unless (identifier? #'name)
       (raise-syntax-error #f "Expected identifier" stx #'name))
     #'(let ([fo f])
         (space (insert-glyph fo (space-glyph ((get-glyph fo 'name) fo) l r)) . spacing-forms))]
    [(space f @ group : l r . spacing-forms)
     (unless (identifier? #'group)
       (raise-syntax-error #f "Expected identifier" stx #'group))
     #'(let ([fo f])
         (space 
          (foldl (lambda (g fo) 
                   (insert-glyph fo (space-glyph ((get-glyph fo g) fo) l r)))
                 fo group)
          . spacing-forms))]
    [(space f name ... : l r . spacing-forms)
     (for-each (lambda (n) (when (not (identifier? n))
                             (raise-syntax-error #f "Expected identifier in inline group" stx n)))
               (syntax->list #'(name ...)))
     #'(let ([group (list 'name ...)])
         (space f @ group : l r . spacing-forms))]))



(define-syntax (space-glyph stx)
  (syntax-case stx (/--/)
    [(_) (raise-syntax-error #f "Expected glyph and spacing forms" stx)]
    [(_ g) (raise-syntax-error #f "Expected spacing forms" stx)]
    [(_ (g f)) (raise-syntax-error #f "Expected spacing forms" stx)]
    [(_ (g f) left-form (/--/ aw))
     (syntax-case #'aw (--)
       [-- #'(let ([adv (advance-width (glyph-advance g))])
               (space-glyph (g f) left-form (/--/ adv)))]
       [w #'(if (not (real? w))
                (error "Expected real? given: " w)
                (struct-copy glyph (space-glyph (g f) left-form --) 
                             [advance (struct-copy advance (glyph-advance g)
                                                   [width w])]))])]
    [(_ (g f) (/--/ aw) r)
     (syntax-case #'aw (--)
       [-- #'(let ([adv (advance-width (glyph-advance g))])
               (space-glyph (g f) (/--/ adv) r))]
       [w #'(if (not (real? w))
                (error "Expected real? given: " w)
                (let* ([ng (space-glyph (g f) -- r)]
                   [adv (advance-width (glyph-advance ng))]
                   [diff (- w adv)])
                  (space-glyph (g f) (<-> diff) (/--/ w))))])]
    [(_ (g f) left-form right-form)
     (with-syntax ([gd #'(let ([fo f])
                           (if fo 
                               (decompose-glyph fo g)
                               g))])
                   
       (let ([make-adj (lambda (s left?)
                         (syntax-case s (-- <->)
                           [-- #'0]
                           [(<-> l) #'(if (not (real? l))
                                          (error "Expected real? given: " l)
                                          l)]
                           [(l h) #`(cond [(not (real? l))
                                           (error "Expected real? given: " l)]
                                          [(not (real? h))
                                           (error "Expected real? given: " h)]
                                          [else (- l ((if #,left? car cdr) (get-sidebearings-at gd h)))])]
                           [l #`(if (not (real? l))
                                          (error "Expected real? given: " l)
                                          (- l ((if #,left? car cdr) (get-sidebearings gd))))]))])
         (with-syntax ([l (make-adj #'left-form #t)]
                       [r (make-adj #'right-form #f)])
           #'(adjust-sidebearings g l r))))]
    [(_ g left-form right-form)
     #'(space-glyph (g #f) left-form right-form)]))
  

; Symbol -> Symbol
; add public.kern1 to the group name
(define (left-kern-group n)
  (string->symbol (~a "public.kern1." n)))

; Symbol -> Symbol
; add public.kern2 to the group name
(define (right-kern-group n)
  (string->symbol (~a "public.kern2." n)))

(define-syntax (kern stx)
  (syntax-case stx  (left-groups right-groups)
    [(_ f [groups  (side (n group) ...) . g] . kern-forms)
     (with-syntax [(ngl #`(lambda (fo)
                            (hash-set* (font-groups fo)
                                       #,@(datum->syntax stx (append* (syntax->datum #'((n group) ...)))))))]       
       (syntax-case #'side (left right)
         [left #'(let ([n (left-kern-group 'n)] ...
                       [fo f])
                   (kern  (struct-copy font fo
                                       [groups (ngl fo)])
                          [groups . g] . kern-forms))]
         [right #'(let ([n (right-kern-group 'n)] ...
                        [fo f])
                    (kern  (struct-copy font fo
                                        [groups (ngl fo)])
                           [groups . g] . kern-forms))]
         [x (raise-syntax-error #f "Group side can be left or right" stx #'x)]))]
    [(_ f [groups] . kern-forms) #'(kern f . kern-forms)]
    [(_ f . kern-forms) #'(let ([fo f]
                                [kh (make-hash)])
                            (struct-copy font fo
                                         [kerning (make-kerns fo kh . kern-forms)]))]))
     

(define-syntax (make-kerns stx)
  (syntax-case stx (: @)
    [(make-kerns f1 #f . kern-forms)
     #'(let ([k (make-hash)])
       (make-kerns f1 k . kern-forms))]
    [(make-kerns f1 kh @ l r : v . kern-forms)
     #'(make-kerns f1 (add-kern kh l 'r v) . kern-forms)] 
    [(make-kerns f1 kh l @ r : v . kern-forms)
     #'(make-kerns f1 (add-kern kh 'l r v) . kern-forms)] 
    [(make-kerns f1 kh @ l @ r : v . kern-forms)
     #'(make-kerns f1 (add-kern kh l r v) . kern-forms)] 
    [(make-kerns f1 kh l r : v . kern-forms)
     (begin
       (unless (identifier? #'l)
         (raise-syntax-error #f "Expected indentifier" stx #'l))
       (unless (identifier? #'r)
         (raise-syntax-error #f "Expected indentifier" stx #'r)))
     #'(make-kerns f1 (add-kern kh 'l 'r v) . kern-forms)] 
    [(make-kerns f1 kh) #'(make-immutable-kerning kh)]))

    
(define-syntax add-kern
  (syntax-rules ()
    [(add-kern k l r v)
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
  (syntax-rules (groups)
    [(define-spacing-rule name (arg ...) (locals ...) [groups (g-name group) ...] body0 . body)
       (define (name font arg ...)
           (let (locals ...)
             (space font
                    [groups (g-name group) ...]
                    body0 . body)))]
    [(define-spacing-rule name (arg ...) (locals ...) body0 . body)
     (define-spacing-rule name (arg ...) (locals ...) [groups] body0 . body)]))


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


      

         
