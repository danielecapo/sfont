#lang racket

(require xml/plist)

(provide (all-defined-out))

(struct dict (entries) #:transparent)
(struct entry (key value) #:transparent)

(define (make-dict . entries)
  (dict entries))
  
(define (dict->plist d)
  (match d
    [(dict entries) (cons 'dict (map dict->plist entries))]
    [(entry k v) (list 'assoc-pair (symbol->string k) (dict->plist v))]
    [(? integer? n) (list 'integer n)]
    [(? real? n) (list 'real n)]
    [(? string? s) s]
    [(list elts ...) (cons 'array (map dict->plist elts))]
    [#f (list 'false)]
    [#t (list 'true)]))
                       

(define (plist->dict pl)
  (match pl
    [(list 'dict entries ...) (dict (map plist->dict entries))]
    [(list 'assoc-pair k v) (entry (string->symbol k) (plist->dict v))]
    [(list 'integer n) n]
    [(list 'real n) n]
    [(? string? s) s]
    [(list 'false) #f]
    [(list 'true) #t]
    [(list 'array elts ...) (map plist->dict elts)]))

(define (dict->hashtable d)
  (define (process-entry e)
    (let ([k (entry-key e)]
          [v (entry-value e)])
      (list k
            (match v
              [(dict _) (dict->hashtable v)]
              [_ v]))))
  (apply hash (append* (map process-entry (dict-entries d)))))
      

(define (hashtable->dict h)
  (if (hash? h)
      (dict (hash-map h (lambda (k v) 
                        (entry k (hashtable->dict v)))))
      h))
  

(define (dict-as-function d)
  (let ([h (dict->hashtable d)])
    (define (aux acc keys)
      (if (null? keys)
          acc
          (aux (hash-ref acc (car keys)) (cdr keys))))
    (lambda (k . keys) (aux h (cons k keys)))))

(define (dict-map d proc)
  (map (lambda (e) (proc (entry-key e) (entry-value e)))
       (dict-entries d)))

(define (dict-for-each d proc)
  (for-each (lambda (e) (proc (entry-key e) (entry-value e)))
       (dict-entries d)))
  
(define (write-dict d path)
  (call-with-output-file path
   (lambda (out) 
     (write-plist (dict->plist d) out))
    #:exists 'replace))

(define (read-dict path)
  (plist->dict (call-with-input-file path read-plist)))