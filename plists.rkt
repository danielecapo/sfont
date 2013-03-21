#lang racket

(require xml/plist)

(provide (all-defined-out))

  
(define (plist->dict pl)
  (match pl
    [(list 'dict entries ...) (map plist->dict entries)]
    [(list 'assoc-pair k v) (cons (string->symbol k) (plist->dict v))]
    [(list 'integer n) n]
    [(list 'real n) n]
    [(? string? s) s]
    [(list 'false) #f]
    [(list 'true) #t]
    [(list 'array elts ...) (map plist->dict elts)]))

(define (dict->plist d)
  (match d
    [(? dict? d) 
     (cons 'dict 
           (dict-map d (lambda (k v) 
                       (list 'assoc-pair (symbol->string k) (dict->plist v)))))]                                       
    [(? exact-integer? n) (list 'integer n)]
    [(? real? n) (list 'real n)]
    [(? string? s) s]
    [(list elts ...) (cons 'array (map dict->plist elts))]
    [#f (list 'false)]
    [#t (list 'true)]))


(define (dict->xexpr d)
  (match d
    [(? dict? d) 
     (cons 'dict 
           (cons null (foldr append '()
                             (dict-map d (lambda (k v) 
                                    (list (list 'key null (symbol->string k))
                                          (dict->xexpr v)))))))]
    
    
    [(? exact-integer? n) (list 'integer null (number->string n))]
    [(? real? n) (list 'real null (number->string n))]
    [(? string? s) (list 'string null s)]
    [(list elts ...) (cons 'array (cons null (map dict->xexpr elts)))]
    [#f (list 'false null)]
    [#t (list 'true null)]))

(define (xexpr->dict x)
  (match x
    [(list 'dict null entries ...) (xexpr->dict entries)]
    [(list (list 'key null k) v entries ...) 
     (cons (cons (string->symbol k) (xexpr->dict v))
           (xexpr->dict entries))]
    [(list 'integer null n) (string->number n)]
    [(list 'real null n) (string->number n)]
    [(list 'string null s) s]
    [(list 'false null) #f]
    [(list 'true null) #t]
    [(list 'array null elts ...) (map xexpr->dict elts)]
    [null null]))

;
;
;
;(define (dict-as-function d)
;  (let ([h (dict->hashtable d)])
;    (define (aux acc keys)
;      (if (null? keys)
;          acc
;          (aux (hash-ref acc (car keys)) (cdr keys))))
;    (lambda (k . keys) (aux h (cons k keys)))))
;

  
(define (write-dict d path)
  (call-with-output-file path
   (lambda (out) 
     (write-plist (dict->plist d) out))
    #:exists 'replace))

(define (read-dict path)
  (plist->dict (call-with-input-file path read-plist)))


;(define g (xml->xexpr 
;   ((eliminate-whitespace '(glyph advance unicode image guideline anchor outline
;                                  contour point component lib dict)
;                          identity)
;   (document-element
;    (call-with-input-file "/Users/daniele/glif.glif" read-xml)))))