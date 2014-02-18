#lang racket
(require "../../main.rkt"
         "../../geometry.rkt"
         "utils.rkt"
         syntax/parse
         (for-syntax racket/syntax
                     racket/list
                     syntax/parse))

(provide
 (contract-out 
  [lowercase-tracy (->* (font? real? real? real? real?) (real? real?) font?)]
  [uppercase-tracy (-> font? real? real? real? real? font?)])
 space 
 space-glyph
 define-spacing-rule)
 

(begin-for-syntax
  (define-syntax-class unchange-form
    #:datum-literals (--)
    #:description "unchange spacing form"
    (pattern --))
  (define-syntax-class advance-form
    #:datum-literals (/--/)
    #:description "advance spacing form"
    (pattern (/--/ val:expr)))
  (define-syntax-class adjust-form
    #:datum-literals (<->)
    #:description "adjust spacing form"
    (pattern (<-> val:expr)))
  (define-syntax-class height-form
    #:datum-literals (^)
    #:description "sidebearing at height spacing form"
    (pattern (^ val:expr height:expr)))
  (define-syntax-class sidebearing-form
    #:description "sidebearing spacing form"
    (pattern val:expr))    
  (define-syntax-class spacing-form
    #:description "spacing form"
    (pattern s:unchange-form)
    (pattern s:advance-form)
    (pattern s:adjust-form)
    (pattern s:height-form)
    (pattern s:sidebearing-form)))


(define-syntax (space stx)
  (define-splicing-syntax-class simple-spacing
    #:description "single glyph spacing"
    #:datum-literals (:)
    (pattern (~seq g:id : left:spacing-form right:spacing-form)))
  (define-splicing-syntax-class class-spacing
    #:description "class spacing"
    #:datum-literals (@ :)
    (pattern (~seq @ g:id : left:spacing-form right:spacing-form)))
  (define-splicing-syntax-class inline-group-spacing
    #:description "inline group spacing"
    #:datum-literals (:)
    (pattern (~seq  (g:id ...) : left:spacing-form right:spacing-form)))
  (define-splicing-syntax-class spacing-rule
    #:description "spacing rule"
    (pattern ss:simple-spacing)
    (pattern cs:class-spacing)
    (pattern is:inline-group-spacing))
  (define-syntax-class groups
    #:description "spacing groups"
    #:datum-literals (groups)
    (pattern (groups (name:id glyphs:expr) ...)))
  
  (syntax-parse stx
    #:datum-literals (:)
    [(_ f:id s:simple-spacing r:expr ...)
     #'(let ([f1 (insert-glyph f (space-glyph (get-glyph f 's.g) s.left s.right))])
         (space f1 r ...))]
    [(_ f:id s:class-spacing r:expr ...)
     #'(let ([gr (hash-ref (font-groups f) 's.g #f)])
         (space 
          (foldl (lambda (gl f) 
                   (insert-glyph f (space-glyph ((get-glyph f gl) f) s.left s.right)))
                 f (if gr gr s.g))
          r ...))]
    [(_ f:id s:inline-group-spacing r:expr ...)     
     #'(let ([group (list 's.g ...)])
         (space f @ group : s.left s.right r ...))]
    [(_ f:id g:groups r:expr ...)
     #'(let ([f1 (struct-copy font f 
                              [groups (apply hash-set* 
                                               (font-groups f)
                                               (append (list 'g.name g.glyphs) ...))])])
           (space f1 r ...))]
    [(_ f:id) #'f]
    [(_ f:id r1:expr r:expr ...) (raise-syntax-error #f "Invalid spacing rule" stx #'r1)]
    [(_ f:expr r:expr ...)
     #'(let ([f1 f])
         (space f1 r ...))]))



    
(define-syntax (space-glyph stx)  
  (syntax-parse stx
    #:datum-literals (-- <-> /--/)
    [(_ (g:id f:id) -- --) #'g]
    [(_ (g:id f:id) l:advance-form r:advance-form)
     (raise-syntax-error #f "duplicated advance rule" stx)]
    [(_ (g:id f:id) left:spacing-form right:advance-form)
     (syntax-parse #'right.val
       [v:unchange-form 
        #'(struct-copy glyph (space-glyph (g f) left --) 
                       [advance (advance (advance-width (glyph-advance g)) 
                                         (advance-height (glyph-advance g)))])]
       [v:expr 
        #'(struct-copy glyph (space-glyph (g f) left --) 
                       [advance (advance v (advance-height (glyph-advance g)))])])]    
    [(_ (g:id f:id) left:advance-form right:spacing-form)
     (syntax-parse #'left.val
       [v:unchange-form 
        #'(let* ([gn (space-glyph (g f) -- right)]
                 [adv (advance-width (glyph-advance gn))])
            (adjust-sidebearings gn (- (advance-width (glyph-advance g)) adv) 0))]
       [v:expr
        #'(let* ([gn (space-glyph (g f) -- right)]
                 [adv (advance-width (glyph-advance gn))])
            (adjust-sidebearings gn (- v adv) 0))])]
    
    [(_ (g:id f:id) left:adjust-form right:spacing-form)
     #'(space-glyph ((adjust-sidebearings g left.val 0) f) -- right)]
    [(_ (g:id f:id) left:spacing-form right:adjust-form)
     #'(space-glyph ((adjust-sidebearings g 0 right.val) f) left --)]
    [(_ (g:id f:id) left:height-form right:spacing-form)
     #'(let ([sb (if f 
                     (get-sidebearings-at g f left.height)
                     (get-sidebearings-at g left.height))])
         (space-glyph ((adjust-sidebearings g (- left.val (car sb)) 0) f) -- right))]
    [(_ (g:id f:id) left:spacing-form right:height-form)
     #'(let ([sb (if f 
                     (get-sidebearings-at g f right.height)
                     (get-sidebearings-at g right.height))])
         (space-glyph ((adjust-sidebearings g 0 (- right.val (cdr sb))) f) left --))]
    [(_ (g:id f:id) left:sidebearing-form right:unchange-form)
     #'(let ([sb (if f 
                     (get-sidebearings g f)
                     (get-sidebearings g))])
         (adjust-sidebearings g (- left.val (car sb)) 0))]
    [(_ (g:id f:id) left:unchange-form right:sidebearing-form)
     #'(let ([sb (if f 
                   (get-sidebearings g f)
                   (get-sidebearings g))])
         (adjust-sidebearings g 0 (- right.val (cdr sb))))]
    [(_ (g:id f:id) left:sidebearing-form right:sidebearing-form)
     #'(let ([sb (if f 
                   (get-sidebearings g f)
                   (get-sidebearings g))])
       (adjust-sidebearings g (- left.val (car sb)) (- right.val (cdr sb))))]
    [(_ (g:id f:expr) left:spacing-form right:spacing-form)
     #'(let ([f1 f])
         (space-glyph (g f1) left right))]
    [(_ (g:expr f:expr) left:spacing-form right:spacing-form)
     #'(let ([g1 g])
         (space-glyph (g1 f) left right))]
    [(_ g:expr left:spacing-form right:spacing-form)
     #'(space-glyph (g #f) left right)]))
    


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
  a : -- (^ b mid)
  b : (^ a mid) e
  c : e f
  d : e (^ a mid)
  e : e f
  f : -- --
  g : -- --
  h : (^ c mid) (^ b mid)
  i : (^ c mid) (^ a mid)
  j : (^ a mid) (^ a mid)
  k : (^ c mid) d
  l : (^ c mid) (^ a mid)
  m : (^ a mid) (^ b mid)
  n : (^ a mid) (^ b mid)
  o : e e
  p : (^ c mid) e
  q : e (^ a mid)
  r : (^ a mid) d
  s : -- --
  t : -- --
  u : (^ b mid) (^ b mid)
  v : d d
  w : d d
  x : d d
  y : (^ d xh) (^ d xh)
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
  B : (^ a mid) c
  C : e c
  D : (^ a mid) e
  E : (^ a mid) c
  F : (^ a mid) c
  G : e (^ b (/ mid 2.5))
  H : (^ a mid) (^ a mid)
  I : (^ a mid) (^ a mid)
  J : d (^ a mid)
  K : (^ a mid) d
  L : (^ a mid) d
  M : (^ b mid) (^ a mid)
  N : (^ b mid) (^ b mid)
  O : e e
  P : (^ a mid) e
  Q : (^ e mid) (^ e mid)
  R : (^ a mid) d
  S : -- --
  T : d d
  U : (^ a mid) (^ b mid)
  V : d d
  W : d d
  X : d d
  Y : d d
  Z : c c)

