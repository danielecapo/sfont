#lang racket
(require "../../main.rkt"
         syntax/parse
         (for-syntax racket/syntax
                     racket/list
                     syntax/parse))

(provide kern)

; Font Symbol (listof Symbol) -> Font
(define (add-to-groups f g gs)
  (struct-copy font f 
               [groups (hash-set (font-groups f) g gs)]))

; Symbol -> Symbol
; add public.kern1 to the group name
(define (left-kern-group n)
  (string->symbol (~a "public.kern1." n)))

; Symbol -> Symbol
; add public.kern2 to the group name
(define (right-kern-group n)
  (string->symbol (~a "public.kern2." n)))

(define (add-to-side-group f n s gs)
  (cond [(eq? s 'side1) (add-to-groups f (left-kern-group n) gs)]
        [(eq? s 'side2) (add-to-groups f (right-kern-group n) gs)]
        [else (raise-syntax-error "Side can be either side1 or side2")]))

(begin-for-syntax
  (define-splicing-syntax-class kern-class-ref
    #:description "kern class reference"
    #:datum-literals (@)
    (pattern (~seq @ k:id)))
  (define-splicing-syntax-class kern-id
    #:description "kern identifier"
    (pattern k:kern-class-ref)
    (pattern (~seq k:id)))
  (define-splicing-syntax-class kern-rule
    #:description "kern rule"
    #:datum-literals (:)
    (pattern (~seq l:kern-id r:kern-id : value:expr))))

(define-syntax (kern stx)
  (define-syntax-class binding-group
    #:description "binding group"
    (pattern (name:id glyphs:expr)))
  (define-syntax-class side-groups
    #:description "side kerning groups"
    (pattern (side g:binding-group ...)
             #:with (name ...) #'(g.name ...)
             #:with (glyphs ...) #'(g.glyphs ...)
             #:fail-unless (or (eq? 'side1 (syntax->datum #'side))
                                (eq? 'side2 (syntax->datum #'side)))
             "Invalid side name"))  
  (define-syntax-class groups
    #:description "kerning groups"
    #:datum-literals (groups)
    (pattern (groups g0:side-groups g1:side-groups)
             #:fail-when (check-duplicate-identifier
                            (list #'g0.side #'g1.side))
                           "duplicate variable name")
    (pattern (groups g0:side-groups)))
  (syntax-parse stx
                [(_ f:id g:groups k:expr ...)
                 (syntax-parse #'g
                   [(groups g0:side-groups g1:side-groups)
                    #'(let ([f1 (foldl (lambda (n gl fo) 
                                         (add-to-side-group fo n 'g0.side gl))
                                       f
                                       (list 'g0.name ...)
                                       (list g0.glyphs ...))])
                        (kern f1 [groups g1] k ...))]
                   [(groups g0:side-groups)
                    #'(let ([f1 (foldl (lambda (n gl fo) 
                                         (add-to-side-group fo n 'g0.side gl))
                                       f
                                       (list 'g0.name ...)
                                       (list g0.glyphs ...))])
                        (kern f1 k ...))])]
                [(_ f:id (~and (~seq k:expr ...)
                               (~seq r:kern-rule ...)))
                 #'(let ([kh (make-hash)])
                     (struct-copy font f
                                  [kerning (make-kerns kh k ...)]))]
                [(_ f:id r:expr ...) (raise-syntax-error #f "Invalid kerning rule" stx #'(r ...))]
                [(_ f:expr r:expr ...)
                 #'(let ([f1 f])
                     (kern f1 r ...))]))

(define-syntax (make-kerns stx)
  (syntax-parse stx
    [(_ k:id kern0:kern-rule r:expr ...)
     (with-syntax ([left (syntax-parse #'kern0.l
                           [(ki:kern-class-ref)
                            #'(left-kern-group 'ki.k)]
                           [(ki:id) #''ki])]
                   [right (syntax-parse #'kern0.r
                            [(ki:kern-class-ref)
                             #'(right-kern-group 'ki.k)]
                            [(ki:id) #''ki])])
       #'(let ([kh (add-kern k left right kern0.value)])
           (make-kerns kh r ...)))]
    [(_ k:id)
     #'(make-immutable-kerning k)]))


(define (add-kern k l r v)
  (begin
    (if (hash-has-key? k l)
        (hash-set! (hash-ref k l) r v)
        (hash-set! k l (make-hash (list (cons r v)))))    
    k))

(define (make-immutable-kerning k)
  (make-immutable-hash
   (hash-map k (lambda (k v)
                 (cons k (make-immutable-hash (hash->list v)))))))

