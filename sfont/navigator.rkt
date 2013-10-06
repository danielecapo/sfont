#lang racket

(require "ufo/ufo-def.rkt"
         "geometry.rkt"
         "utilities.rkt"
         (for-syntax racket/syntax))
(require "../sfont-examples/fontwrite-square.rkt")

(provide fref
         fset
         fupdate)

   

(define (lookup o field . args)
  (letrec ([aux (lambda (t)
                  (if (null? t)
                      (error "The object can be accessed")
                      (if ((caar t) o)
                          (let ([ac (assoc field (cdar t))])
                            (if ac 
                                (apply (cadr ac) o args)
                                (error "Invalid field")))
                          (aux (cdr t)))))])
    (aux navigator)))

(define (lookup-set o field . args)
  (letrec ([aux (lambda (t)
                  (if (null? t)
                      (error (format "The object ~a can't be accessed" o))
                      (if ((caar t) o)
                          (let ([ac (assoc field (cdar t))])
                            (if ac 
                                (apply (cadr ac) o args)
                                (error "Invalid field")))
                          (aux (cdr t)))))])
    (aux setter)))

(define-syntax (fref stx)
  (syntax-case stx (--> @)
    [(_ o (field --> proc0 . procs)) 
     #'((apply compose (reverse (list proc0 . procs)))  (lookup o 'field))]
    [(_ o (field @ i)) 
     #'(sequence-ref (lookup o 'field) i)]
    [(_ o (field arg0 . args)) #'(lookup o 'field arg0 . args)]
    [(_ o field) 
     (unless (identifier? #'field) 
       (raise-syntax-error #f "Expected identifier" stx #'field))
     #'(lookup o 'field)]
    [(_ o field0 field ...) #'(==> (fref o field0) (fref field ...))]))

(define (set-in-sequence s i v)
  (cond [(list? s)
         (if (number? i)
             (append (take s i)
                     (cons v (drop s (add1 i))))
             (error (format "The index ~a is not a number" i)))]
        [else (dict-set s i v)]))
             
         

(define-syntax (fset stx)
  (syntax-case stx (--> @)
    [(_ (o (field @ i)) v)
     #'(fset (o field) (set-in-sequence (fref o field) i v))]
    [(_ (o (field arg0 . args)) v)
     #'((lookup-set o 'field arg0 . args) v)]
    [(_ (o field) v) 
     (unless (identifier? #'field) 
       (raise-syntax-error #f "Expected identifier" stx #'field))
     #'((lookup-set o 'field) v)]
    [(_ (o field0 field ...) v) 
     #'(fset (o field0) 
                 (fset ((fref o field0) field ...) v))]
    ))

(define-syntax (fupdate stx)
  (syntax-case stx ()
    [(_ (o field0 field ...) proc)
     #'(fset (o field0 field ...)
                 (proc (fref o field0 field ...)))]))

(define navigator
  `((,font? 
     (format     ,font-format)
     (creator    ,font-creator)
     (fontinfo   ,font-fontinfo)
     (groups     ,font-groups)
     (kerning    ,font-kerning)
     (features   ,font-features)
     (layers     ,font-layers)
     (lib        ,font-lib)
     (data       ,font-data)
     (images     ,font-images)
     (layer      ,get-layer)
     (glyph      ,get-glyph))
    (,layer?
     (name       ,layer-name)
     (info       ,layer-info)
     (glyphs     ,layer-glyphs)
     (glyph      ,get-glyph))
   (,glyph?
    (format      ,glyph-format)
    (name        ,glyph-name)
    (advance     ,glyph-advance)
    (unicodes    ,glyph-unicodes)
    (note        ,glyph-note)
    (image       ,glyph-image)
    (guidelines  ,glyph-guidelines)
    (anchors     ,glyph-anchors)
    (contours    ,glyph-contours)
    (components  ,glyph-components)
    (lib         ,glyph-lib))
   (,advance?
    (width       ,advance-width)
    (height      ,advance-height))
   (,image?
    (filename   ,image-filename)
    (matrix     ,image-matrix)
    (color      ,image-color))
   (,guideline? 
    (pos        ,guideline-pos) 
    (angle      ,guideline-angle) 
    (name       ,guideline-name)
    (color      ,guideline-color) 
    (identifier ,guideline-identifier))
   (,anchor?
    (pos        ,anchor-pos)
    (name       ,anchor-name)
    (color      ,anchor-color)
    (identifier ,anchor-identifier))
   (,contour?
    (identifier ,contour-identifier)
    (points     ,contour-points))
   (,component?
    (base       ,component-base)
    (matrix     ,component-matrix)
    (identifier ,component-identifier))
   (,point? 
    (pos        ,point-pos)
    (type       ,point-type)
    (smooth     ,point-smooth)
    (name       ,point-name)
    (identifier ,point-identifier))
   (,vec? 
    (x          ,vec-x)
    (y          ,vec-y))
   (,trans-mat?
    (x          ,trans-mat-x)
    (xy         ,trans-mat-xy)
    (yx         ,trans-mat-yx)
    (y          ,trans-mat-y)
    (x-offset   ,trans-mat-x-offset)
    (y-offset   ,trans-mat-y-offset))))


(define-syntax (setters stx)
  (syntax-case stx ()
    [(_ str field ...)
     (with-syntax ([pred (format-id stx "~a?" #'str)])
     #'(list pred (list 'field (lambda (o) (lambda (v) (struct-copy str o [field v])))) ...))]))
             
(define-syntax (getters stx)
  (syntax-case stx ()
    [(_ str field ...)
     (with-syntax ([pred (format-id stx "~a?" #'str)]
                   [(acc ...) (map (lambda (f) (format-id stx "~a-~a" #'str f)) (syntax->list #'(field ...)))])
     #'(list pred (list 'field acc) ...))]))
             
        
; (Font or Layer) Symbol Symbol-> (Glyph -> Font)
(define (glyph-set o g)
  (cond [(font? o)
         (lambda (gl)
           (insert-glyph o 
                         (struct-copy glyph gl
                                      [name g]
                                      [unicodes (unicode g)])))]
        [(layer? o)
         (lambda (gl)
           (struct-copy layer o
                        [glyphs (hash-set (layer-glyphs o) g
                                          (struct-copy glyph gl
                                                       [name g]
                                                       [unicodes (unicode g)]))]))]))

(define setter
  (list
   (setters point pos type smooth name identifier)
   (setters contour identifier points) 
   (setters component base matrix identifier)
   (setters anchor pos name color identifier)
   (setters guideline pos angle name color identifier)
   (setters image filename matrix color)
   (setters advance width height)
   (setters glyph format name advance unicodes note image
            guidelines anchors contours components lib)
   (append (setters layer name info glyphs)
           `((glyph ,glyph-set)))
   (append (setters font format creator fontinfo groups kerning 
                    features layers lib data images)
           `((glyph ,glyph-set)
             (layer ,(lambda (f l)
                       (lambda (nl)
                         (set-layer f (struct-copy layer nl [name l])))))))
                                                                  
   (setters vec x y)
   (setters trans-mat x xy yx y x-offset y-offset)))