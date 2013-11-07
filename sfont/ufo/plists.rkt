#lang racket

(require xml/plist
         xml/path
         xml)

(provide
 (contract-out
  [dict-plist? (-> any/c boolean?)]
  [plist->dict (-> plist-value? dict-plist?)]
  [dict->plist (-> dict-plist? plist-value?)]
  [dict->xexpr (-> dict-plist? xexpr/c)]
  [xexpr->dict (-> xexpr/c dict-plist?)]
  [write-dict (-> dict-plist? path-string? any)]
  [read-dict (-> path-string? dict-plist?)]))

   
(define (dict-plist? d)
  (or (hash? d)
      (list? d)
      (exact-integer? d)
      (real? d)
      (string? d)
      (boolean? d)))


; Plist -> HashtablePlist
; produce an hashtable from the plist
(define (plist->dict pl)
  (match pl
    [(list 'dict entries ...) (make-immutable-hash (map plist->dict entries))]
    [(list 'assoc-pair k v) (cons (string->symbol k) (plist->dict v))]
    [(list 'integer n) n]
    [(list 'real n) n]
    [(? string? s) s]
    [(list 'false) #f]
    [(list 'true) #t]
    [(list 'array elts ...) (map plist->dict elts)]
    [_ (error (~a "plist->dict: " pl " is an invalid plist"))]))

; DictPlist -> Plist
; produce a plist from a dict plist
(define (dict->plist d)
  (match d
    [(list) (list 'array)]
    [(list elts ...) (cons 'array (map dict->plist elts))]
    [(? hash? d) 
     (cons 'dict 
           (dict-map d (lambda (k v) 
                       (list 'assoc-pair (symbol->string k) (dict->plist v)))))]                                       
    [(? exact-integer? n) (list 'integer n)]
    [(? real? n) (list 'real n)]
    [(? string? s) s]
    [#f (list 'false)]
    [#t (list 'true)]
    [_ (error (~a "dict->plist: " d "can't be converted in a plist"))]))

; DictPlist -> Xexpr
; produce a xexpr representation of an dict plist
(define (dict->xexpr d)
  (match d
    [(list) (list 'array null)]
    [(list elts ...) (cons 'array (cons null (map dict->xexpr elts)))]
    [(? hash? d) 
     (cons 'dict 
           (cons null (foldr append '()
                             (dict-map d (lambda (k v) 
                                    (list (list 'key null (symbol->string k))
                                          (dict->xexpr v)))))))]
    
    
    [(? exact-integer? n) (list 'integer null (number->string n))]
    [(? real? n) (list 'real null (number->string n))]
    [(? string? s) (list 'string null s)]
    [#f (list 'false null)]
    [#t (list 'true null)]
    [_ (error (~a "dict->xexpr: " d "can't be converted in a plist"))]))

; Xexpr -> DictPlist
; produce an dict plist from a xexpr
(define (xexpr->dict x)
  (match x
    [(list 'dict null entries ...) (make-immutable-hash (xexpr->dict entries))]
    [(list (list 'key null k) v entries ...) 
     (cons (cons (string->symbol k) (xexpr->dict v))
           (xexpr->dict entries))]
    [(list 'integer null n) (string->number n)]
    [(list 'real null n) (string->number n)]
    [(list 'string null s) s]
    [(list 'string null) ""]
    [(list 'false null) #f]
    [(list 'true null) #t]
    [(list 'array null elts ...) (map xexpr->dict elts)]
    [null null]
    [_ (error (~a "xexpr->dict: " x " is an invalid plist"))]))

; DictPlist Path -> Any
; write the dict plist to file
(define (write-dict d path)
  (call-with-output-file path
   (lambda (out) 
     (write-plist (dict->plist d) out))
    #:exists 'replace))

; Path -> DictPlist
; produce an dict plist reading from a file
(define (read-dict path)
     (xexpr->dict
      (se-path* '(plist)
                (xml->xexpr 
                 ((eliminate-whitespace 
                   '(plist lib dict array)
                   identity)
                  (document-element
                   (call-with-input-file path read-xml)))))))
  
 