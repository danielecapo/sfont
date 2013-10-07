#lang racket

(require "ufo/ufo-def.rkt"
         "geometry.rkt"
         "utilities.rkt"
         (for-syntax racket/syntax))

(provide fref
         fset
         fupdate)

   
; (listof Pairs) Any/c Symbol Any/c ... -> Any/c
(define (lookup dic o field . args)
  (letrec ([aux (lambda (t)
                  (if (null? t)
                      (error "The object can be accessed")
                      (if ((caar t) o)
                          (let ([ac (assoc field (cdar t))])
                            (if ac 
                                (apply (cadr ac) o args)
                                (error "Invalid field")))
                          (aux (cdr t)))))])
    (aux dic)))


(define-syntax (fref stx)
  (syntax-case stx (--> @)
    [(_ o (field --> proc0 . procs)) 
     #'((apply compose (reverse (list proc0 . procs)))  (lookup getter o 'field))]
    [(_ o (field @ i)) 
     #'(sequence-ref (lookup getter o 'field) i)]
    [(_ o (field arg0 . args)) #'(lookup getter o 'field arg0 . args)]
    [(_ o field) 
     (unless (identifier? #'field) 
       (raise-syntax-error #f "Expected identifier" stx #'field))
     #'(lookup getter o 'field)]
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
     #'((lookup setter o 'field arg0 . args) v)]
    [(_ (o field) v) 
     (unless (identifier? #'field) 
       (raise-syntax-error #f "Expected identifier" stx #'field))
     #'((lookup setter o 'field) v)]
    [(_ (o field0 field ...) v) 
     #'(fset (o field0) 
                 (fset ((fref o field0) field ...) v))]
    ))

(define-syntax (fupdate stx)
  (syntax-case stx ()
    [(_ (o field0 field ...) proc)
     #'(fset (o field0 field ...)
                 (proc (fref o field0 field ...)))]))


(define-syntax (getters stx)
  (syntax-case stx ()
    [(_ str field ...)
     (with-syntax ([pred (format-id stx "~a?" #'str)]
                   [(acc ...) (map (lambda (f) (format-id stx "~a-~a" #'str f)) (syntax->list #'(field ...)))])
     #'(list pred (list 'field acc) ...))]))

(define getter
  (list
   (getters point pos type smooth name identifier)
   (getters contour identifier points) 
   (getters component base matrix identifier)
   (getters anchor pos name color identifier)
   (getters guideline pos angle name color identifier)
   (getters image filename matrix color)
   (getters advance width height)
   (getters glyph format name advance unicodes note image
            guidelines anchors contours components lib)
   (append (setters layer name info glyphs)
           `((glyph ,get-glyph)))
   (append (setters font format creator fontinfo groups kerning 
                    features layers lib data images)
           `((glyph ,get-glyph)
             (layer ,get-layer)))
                                                                  
   (getters vec x y)
   (getters trans-mat x xy yx y x-offset y-offset)))




(define-syntax (setters stx)
  (syntax-case stx ()
    [(_ str field ...)
     (with-syntax ([pred (format-id stx "~a?" #'str)])
     #'(list pred (list 'field (lambda (o) (lambda (v) (struct-copy str o [field v])))) ...))]))
             

             
        
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