#lang racket

(require "private/ufo/ufo-def.rkt"
         "geometry.rkt"
         "utilities.rkt"
         (for-syntax racket/syntax
                     syntax/parse))

(provide fref
         fset
         fupdate)

   
; (listof Pairs) Any/c Symbol Any/c ... -> Any/c
(define (lookup dic o field . args)
  (letrec ([aux (lambda (t)
                  (if (null? t)
                      (error "The object can't be accessed")
                      (if ((caar t) o)
                          (let ([ac (assoc field (cdar t))])
                            (if ac 
                                (apply (cadr ac) o args)
                                (error "Invalid field")))
                          (aux (cdr t)))))])
    (aux dic)))


(define-syntax (fref stx)
  (syntax-parse stx 
    #:datum-literals (--> @)
    [(_ o:expr (field:id --> proc0:expr . procs)) 
     #'((apply compose (reverse (list proc0 . procs)))  (fref o field))]
    [(_ o:expr (field:id @ i:expr)) 
     #'(let ([s (lookup getter o 'field)])
         (cond [(dict? s) (dict-ref s i)]
               [(list? s) (list-ref s i)]))]
    [(_ o:expr (field:id arg0:expr . args)) #'(lookup getter o 'field arg0 . args)]
    [(_ o:expr field:id) 
     #'(lookup getter o 'field)]
    [(_ o:expr field0:expr field:expr ...) #'(==> (fref o field0) (fref field ...))]))

(define (set-in-sequence s i v)
  (cond [(list? s)
         (if (number? i)
             (append (take s i)
                     (cons v (drop s (add1 i))))
             (error (format "The index ~a is not a number" i)))]
        [else (dict-set s i v)]))
             
         

(define-syntax (fset stx)
  (syntax-parse stx  
    #:datum-literals (@)
    [(_ (o:expr (field:id @ i:expr)) v:expr)
     #'(fset (o field) (set-in-sequence (fref o field) i v))]
    [(_ (o:expr (field:id arg0:expr . args)) v:expr)
     #'((lookup setter o 'field arg0 . args) v)]
    [(_ (o:expr field:id) v:expr) 
     #'((lookup setter o 'field) v)]
    [(_ (o:expr field0:expr field:expr ...) v:expr) 
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
   (append (getters glyph name advance unicodes 
                    note image layers lib)
           `((layer ,get-layer)))
   (getters layer guidelines anchors contours components)
   (getters layer-info name color lib)
   (append (getters font fontinfo groups kerning 
                    features glyphs layers lib data images)
           `((glyph ,get-glyph)))                                                                  
   (getters vec x y)
   (getters trans-mat x xy yx y x-offset y-offset)))




(define-syntax (setters stx)
  (syntax-case stx ()
    [(_ str field ...)
     (with-syntax ([pred (format-id stx "~a?" #'str)])
     #'(list pred (list 'field (lambda (o) (lambda (v) (struct-copy str o [field v])))) ...))]))
             

             
        
; Font Symbol -> (Glyph -> Font)
(define (glyph-set f g)
  (lambda (gl)
    (insert-glyph f (struct-copy glyph gl
                                 [name g]
                                 [unicodes (unicode g)]))))

(define setter
  (list
   (setters point pos type smooth name identifier)
   (setters contour identifier points) 
   (setters component base matrix identifier)
   (setters anchor pos name color identifier)
   (setters guideline pos angle name color identifier)
   (setters image filename matrix color)
   (setters advance width height)
   (append (setters glyph name advance unicodes 
                    note image layers lib)
           `((layer ,(lambda (g l) (lambda (nl) (set-layer g (struct-copy layer nl [name l])))))))
   (setters layer guidelines anchors contours components)
   (setters layer-info name color lib)
   (append (setters font fontinfo groups kerning 
                    features glyphs layers lib data images)
           `((glyph ,glyph-set)))                                                                  
   (setters vec x y)
   (setters trans-mat x xy yx y x-offset y-offset)))