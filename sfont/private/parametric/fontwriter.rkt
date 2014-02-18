#lang racket
(require "../../geometry.rkt"
         "../../main.rkt"
         "../../utilities.rkt"
         "../spacing/space.rkt"
         (for-syntax racket/list
                     syntax/parse))


(provide 
 (contract-out
  [alignment/c (-> any/c boolean?)]
  [alg (-> alignment/c real?)]
  [ovs (-> alignment/c real?)]
  [ovs-height (-> alignment/c real?)])
 glyph.
 font.
 from
 translate.
 rotate.
 scale.
 skew-x.
 skew-y.
 reflect-x.
 reflect-y.
 )


          



; Syntax for defining geometric transformations
(define-syntax-rule (define-transform name fn)
  (define-syntax name
    (syntax-rules (from)
      [(name o from (x y) . args)
       (from (x y) (name o . args))]
      [(name o . args)
       (cond [(list? o) (map (lambda (i) (fn i . args)) o)]
             [else (fn o . args)])])))



(define-transform translate. translate)
(define-transform rotate. rotate)
(define-transform scale. scale)
(define-transform skew-x. skew-x)
(define-transform skew-y. skew-y)
(define-transform reflect-x. reflect-x)
(define-transform reflect-y. reflect-y)


(define-syntax-rule (from (x y) (fn o . args))
  (translate. (fn (translate. o (- x) (- y)) . args)
              x y))





(define-syntax (glyph. stx)
  (define-syntax-class glyph-metrics-form
    #:description "glyph spacing definitions"
    #:datum-literals (metrics)
    (pattern (metrics left:expr right:expr)))  
  
  
  (define-syntax-class binding
      #:description "binding pair"
      (pattern (var:id v:expr)))

  (define-syntax-class locals-form
    #:description "local variable bindings"
    #:datum-literals (locals)
    (pattern (locals b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
             "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (v ...) #'(b.v ...)))
  
  (define-splicing-syntax-class maybe-locals
    #:description "local variable bindings"
    #:datum-literals (locals)
    (pattern (~seq))
    (pattern (~seq l:locals-form)))
  
  (define-syntax-class glyph-contours
    #:description "glyph contours"
    #:datum-literals (metrics)
    (pattern (contours cnt:expr ...)))
  (define-splicing-syntax-class maybe-contours
    #:description "glyph contours"
    #:datum-literals (metrics)
    (pattern (~seq cnt:glyph-contours))
    (pattern (~seq)))
  
  (syntax-parse stx
    #:datum-literals (metrics contours locals)
    [(glyph. name:expr 
             loc:maybe-locals
             met:glyph-metrics-form
             contour-form:maybe-contours)
     (syntax-parse #'loc
       [()
        (with-syntax ([cnts (syntax-parse #'contour-form
                              [() #'(list)]
                              [(c:glyph-contours)
                               #'(map bezier->contour 
                                      (build-contour-list c.cnt ...))])])
        #'(space-glyph 
           (glyph name (advance 0 0) (unicode name) #f #f 
                  (list (layer foreground null null cnts null))
                  (make-immutable-hash))
           met.left met.right))]
       [(l:locals-form)
        (with-syntax ([g (syntax-parse #'contour-form
                           [() #'(lambda (n) (glyph. n met))]
                           [(c:glyph-contours)
                            #'(lambda (n) (glyph. n met c))])])
        #'(let* ([gname name]
                 [l.var l.v] ...)
            (g gname)))])]))
      
            
;        
;     #'(glyph. name 
;               [locals]
;               [metrics left-form right-form]
;               . contour-form)]
;    [(glyph. name 
;             [locals [s v] ...]
;             [metrics left-form right-form]
;             . contour-form)
;     (with-syntax ([cnts (syntax-case #'contour-form (contours)
;                           [() #'(list)]
;                           [((contours cnt . cnts))
;                            #'(map bezier->contour 
;                                   (build-contour-list cnt . cnts))])])
;       #'(let* ([s v] ...)
;           (space-glyph
;            (glyph name (advance 0 0) (unicode name) #f #f 
;                   (list (layer foreground null null cnts null))
;                   (make-immutable-hash))
;            left-form right-form)))]
;    [(glyph. . body) (raise-syntax-error #f "Invalid glyph. definition." stx)]))



; (Bezier or (listOf Bezier) ... -> (listOf Bezier)
(define/contract (build-contour-list . cnts)
  (->* () () #:rest (listof (or/c bezier/c (listof bezier/c))) (listof bezier/c))
  (foldl 
   (lambda (c r)
     (append r (if (bezier/c c)
                   (list c)
                   c)))
   '()
   cnts))




; Alignemnt is a list of two elements, the first element is the position, the second represents the height of overshoot
(define alignment/c (flat-named-contract 'alignment/c (list/c real? real?)))

; Alignment -> Real
; return the position of alignment
(define (alg al)
  (car al))



; Alignment -> Real
; return the position of overshoot for Alignment
(define (ovs al)
  (+ (alg al) (ovs-height al)))


; Alignment -> Real
; return the height of overshoot for Alignment
(define (ovs-height al)
  (cadr al))


(define-syntax emit-font-form
  (syntax-rules ()
    [(emit-font-form name 
                     ascender-id 
                     descender-id
                     (blue ...)
                     (v ...)
                     glyph-form ...)
     
     (let* (v ...)
       (font (make-immutable-hash
              (let* ([all-blues (list (list (alg blue) (ovs blue)) ...)]
                     [blues (sort (flatten 
                                   (filter ((curry ormap) 
                                            (negate negative?)) 
                                           all-blues))
                                  <)]
                     [o-blues (sort (flatten 
                                     (filter ((curry andmap) negative?)
                                             all-blues))
                                    <)]
                     [infoa (list (cons 'unitsPerEm (+ (alg ascender-id) (- (alg descender-id))))
                                  (cons 'ascender (alg ascender-id))
                                  (cons 'descender (alg descender-id))
                                  (cons 'familyName (symbol->string (quote name)))
                                  (cons 'postscriptFontName (symbol->string (quote name)))
                                  (cons 'versionMajor 1)
                                  (cons 'versionMinor 0))]
                     [infob (if (null? blues)
                                infoa
                                (cons (cons 'postscriptBlueValues blues)
                                      infoa))])
                (if (null? o-blues)
                    infob
                    (cons (cons 'postscriptBlueValues blues)
                          infob))))
             (make-immutable-hash) 
             (make-immutable-hash) 
             #f
             (build-glyphs-list glyph-form ...)
             (list 
              (layer-info foreground #f (make-immutable-hash)))
             (make-immutable-hash)
             #f #f))]))


; (Glyph or (listOf Glyph)) ... -> (listOf Glyph)
(define/contract (build-glyphs-list . glyphs)
  (->* () () #:rest (listof (or/c glyph? (listof glyph?))) (listof glyph?))
  (foldl (lambda (g r) (if (list? g) 
                           (append r g)
                           (append r (list g))))
         '()
         glyphs))



(define-syntax (font. stx)
  (syntax-case stx (alignments variables glyphs)
    [(font. (name [param dflt] ...) . rest)
     (for-each (lambda (p) 
                 (unless (identifier? p)
                   (raise-syntax-error #f "Expected identifier" stx p)))
               (syntax->list #'(param ...)))
     (with-syntax ([kwarglist
                    (datum->syntax stx
                                   (append*
                                    (map (lambda (p d)
                                           (cons (string->keyword (symbol->string (syntax->datum p))) (list (list p d))))
                                         (syntax->list #'(param ...))
                                         (syntax->list #'(dflt  ...)))))])
       #'(lambda kwarglist (font. name . rest)))]
    [(font. name
            [alignments als ...]
            [variables v ...]
            [glyphs glyph-form ...])
     (for-each (lambda (i) (unless (identifier? i)
                             (raise-syntax-error #f "Expected identifier" stx #'i)))
               (cons #'name
                     (append (map (compose car syntax->list) (syntax->list #'(als ...)))
                             (map (compose car syntax->list) (syntax->list #'(v ...))))))
     (letrec ([find-blues
               (lambda (s acc)
                 (syntax-case s (:font-ascender :font-descender)
                   [() acc]
                   [([n a o :font-ascender] . as) (find-blues #'as acc)]
                   [([n a o :font-descender] . as) (find-blues #'as acc)]
                   [([n a o] . as) (find-blues #'as (datum->syntax s (cons #'n (syntax->list acc))))]))]
              [find-ascender
               (lambda (s)
                 (syntax-case s (:font-ascender)
                   [() (raise-syntax-error #f "Font doesn't define an alignment to be used as ascender in fontinfo" #'(alignments als ...))]
                   [([n a o :font-ascender] . as) #'n]
                   [([n a o] . as) (find-ascender #'as)]
                   [([n a o r] . as) (find-ascender #'as)]))]
              [find-descender
               (lambda (s)
                 (syntax-case s (:font-descender)
                   [() (raise-syntax-error #f "Font doesn't define an alignment to be used as descender in fontinfo" #'(alignments als ...))]
                   [([n a o :font-descender] . as) #'n]
                   [([n a o . r] . as) (find-descender #'as)]))])
       (with-syntax ([ascender  (find-ascender  #'(als ...))]
                     [descender (find-descender #'(als ...))]
                     [((alg-name a o . r) ...) #'(als ...)]
                     [(blue ...) (find-blues #'(als ...) #'())])
         #'(let* ([alg-name (list a o)] ...)
             (emit-font-form name 
                             ascender 
                             descender 
                             (blue ...)
                             (v ...) 
                             glyph-form ...))))]
    [(font. name
            [alignments als ...]
            [glyphs glyph-form ...])
     #'(font. name [alignments als ...] [variables] [glyphs glyph-form ...])]))



