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
 component.
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

(begin-for-syntax
  (define-syntax-class binding
      #:description "binding pair"
      (pattern (var:id v:expr))))

(define-syntax (component. stx)
  (syntax-parse stx
   [(_ base:expr (sx:expr sxy:expr syx:expr sy:expr ox:expr oy:expr))
    #'(component base (trans-mat sx sxy syx sy ox oy) #f)]
   [(_ base:expr)
    #'(component base (trans-mat 1 0 0 1 0 0) #f)]))

(define-syntax (glyph. stx)
  (define-syntax-class glyph-metrics-form
    #:description "glyph spacing definitions"
    #:datum-literals (metrics)
    (pattern (metrics left:expr right:expr)))  

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
    #:datum-literals (contours)
    (pattern (contours cnt:expr ...)))
  (define-splicing-syntax-class maybe-contours
    #:description "glyph contours"
    (pattern (~seq cnt:glyph-contours))
    (pattern (~seq)))
  (define-syntax-class glyph-components
    #:description "glyph components"
    #:datum-literals (components)
    (pattern (components cmp:expr ...)))
  (define-splicing-syntax-class maybe-components
    #:description "glyph components"
    (pattern (~seq cmp:glyph-components))
    (pattern (~seq)))
  
  (syntax-parse stx
    #:datum-literals (metrics contours locals)
    [(glyph. name:expr 
             loc:maybe-locals
             met:glyph-metrics-form
             r:expr ...)
     (let ([layer-contents
            (foldl 
             (lambda (e acc)
               (syntax-parse e
               [c:glyph-contours
                (hash-set acc 'contours 
                          #'(map bezier->contour 
                                 (build-contour-list c.cnt ...)))]
               [c:glyph-components 
                (hash-set acc 'components
                          #'(build-component-list c.cmp ...))]))
             (hash 'contours #'null 'components #'null)
             (syntax->list #'(r ...)))])
       (with-syntax ([cnts (hash-ref layer-contents 'contours)]
                     [cmps (hash-ref layer-contents 'components)])
         (syntax-parse #'loc
           [()
            #'(space-glyph 
               (glyph name (advance 0 0) (unicode name) #f #f 
                      (list (layer foreground null null cnts cmps))
                      (make-immutable-hash))
             met.left met.right)]
         [(l:locals-form)
          #'(let* ([gname name]
                     [l.var l.v] ...)
                (space-glyph 
                 (glyph gname (advance 0 0) (unicode name) #f #f 
                        (list (layer foreground null null cnts cmps))
                        (make-immutable-hash))
                 met.left met.right))])))]))
      


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

; (Component or (listOf Component) ... -> (listOf Component)
(define/contract (build-component-list . cmps)
  (->* () () #:rest (listof (or/c component? (listof component?))) (listof component?))
  (foldl 
   (lambda (c r)
     (append r (if (component? c)
                   (list c)
                   c)))
   '()
   cmps))




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
                     (glyph-form ...)
                     (spc ...))
     
     (let* (v ...)
       (let ([f (font (make-immutable-hash
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
                      #f #f)])
         (space f spc ...)))]))


; (Glyph or (listOf Glyph)) ... -> (listOf Glyph)
(define/contract (build-glyphs-list . glyphs)
  (->* () () #:rest (listof (or/c glyph? (listof glyph?))) (listof glyph?))
  (foldl (lambda (g r) (if (list? g) 
                           (append r g)
                           (append r (list g))))
         '()
         glyphs))



(define-syntax (font. stx)
;  (define-syntax-class parameter-form
;    #:description "parameters"
;    (pattern (b:binding ...)
;             #:fail-when (check-duplicate-identifier
;                          (syntax->list #'(b.var ...)))
;             "duplicate parameter name"
;             #:with (para ...) #'(b.var ...)
;             #:with (dflt ...) #'(b.v ...)))
;  (define-splicing-syntax-class parameters
;    #:description "parameters"
;    (pattern (~seq 
  
  (define-syntax-class alignment-form
    #:description "alignment"
    (pattern (name:id align:expr ovs:expr)))
  (define-syntax-class ascender-alignment-form
    #:description "ascender alignment"
    (pattern (name:id align:expr ovs:expr #:font-ascender)))
  (define-syntax-class descender-alignment-form
    #:description "descender alignment"
    (pattern (name:id align:expr ovs:expr #:font-descender)))
  (define-syntax-class general-alignment-form
    #:description "alignment"
    (pattern al:alignment-form
             #:with name #'al.name
             #:with align #'al.align
             #:with ovs #'al.ovs)
    (pattern al:ascender-alignment-form
             #:with name #'al.name
             #:with align #'al.align
             #:with ovs #'al.ovs)
    (pattern al:descender-alignment-form
             #:with name #'al.name
             #:with align #'al.align
             #:with ovs #'al.ovs))
  (define-syntax-class spacing
    #:datum-literals (spacing)
    #:description "spacing"
    (pattern (spacing spc:expr ...)))
  (define-splicing-syntax-class maybe-spacing
    #:description "spacing"
    (pattern (~seq))
    (pattern (~seq s:spacing)))
  
  (syntax-parse stx 
    #:datum-literals (alignments variables glyphs spacing)
    [(font. (name:id p:binding ...) . rest)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(p.var ...)))
     "duplicate parameter name"
     (with-syntax ([kwarglist
                    (datum->syntax stx
                                   (append*
                                    (map (lambda (p d)
                                           (cons (string->keyword (symbol->string (syntax->datum p))) (list (list p d))))
                                         (syntax->list #'(p.var ...))
                                         (syntax->list #'(p.v  ...)))))])
       #'(lambda kwarglist (font. name . rest)))]
    [(font. name
            [alignments als:general-alignment-form ...]
            [variables v:binding ...]
            [glyphs glyph-form ...]
            s:maybe-spacing)
     (letrec ([find-blues
               (lambda (s acc)
                 (syntax-parse s 
                   [() acc]
                   [(al:ascender-alignment-form . as) (find-blues #'as acc)]
                   [(al:descender-alignment-form . as) (find-blues #'as acc)]
                   [(al:alignment-form . as) (find-blues #'as (datum->syntax s (cons #'al.name (syntax->list acc))))]))]
              [find-ascender
               (lambda (s)
                 (syntax-parse s 
                   [() (raise-syntax-error #f "Font doesn't define an alignment to be used as ascender in fontinfo" #'(alignments als ...))]
                   [(al:ascender-alignment-form . as) #'al.name]
                   [(_ . as) (find-ascender #'as)]))]
              [find-descender
               (lambda (s)
                 (syntax-parse s 
                   [() (raise-syntax-error #f "Font doesn't define an alignment to be used as descender in fontinfo" #'(alignments als ...))]
                   [(al:descender-alignment-form . as) #'al.name]
                   [(_ . as) (find-descender #'as)]))])
       (with-syntax ([ascender  (find-ascender  #'(als ...))]
                     [descender (find-descender #'(als ...))]
                     
                     [(blue ...) (find-blues #'(als ...) #'())]
                     [(spc ...) (syntax-parse #'s
                                 [() #'()]
                                 [(sp:spacing) #'(sp.spc ...)])])
         #'(let* ([als.name (list als.align als.ovs)] ...)
             (emit-font-form name 
                             ascender 
                             descender 
                             (blue ...)
                             (v ...) 
                             (glyph-form ...)
                             (spc ...)))))]
               
    [(font. name
            [alignments als:general-alignment-form ...]
            [glyphs glyph-form ...]
            r ...)
     #'(font. name [alignments als ...] [variables] [glyphs glyph-form ...] r ...)]))




