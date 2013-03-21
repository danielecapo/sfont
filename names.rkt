#lang racket

(provide name->filename
         namesymbol->filename)

(define (purge-illegal-char s)
  (let ([charlist (string->list s)]
        [illegal (string->list "\"*+/:<>?[\\]|")])
    (list->string
     (map (lambda (c)
            (if (or (foldl (lambda (a b) (or b (char=? a c))) #f illegal)
                    (< (char->integer c) #x1f)
                    (= (char->integer c) #x7f))
                #\_
                c))
          charlist))))
         
  
(define (trim-to-max-length s max-len)
  (if (> (string-length s) max-len)
      (substring s 0 max-len)
      s))


 
(define (start-with? s char)
  (char=? (string-ref s 0) char))

(define (initial-dot? s)
  (start-with? s #\.))

(define (purge-initial-dot has-not-prefix? s)
  (if (and has-not-prefix? (initial-dot? s))
      (string-append "_" (substring s 1))
      s))

(define (break-at-dot s)
  (string-split s "."))

(define (break-at-underscore s)
  (string-split s "_"))

(define (start-with-uppercase? s)
  (char-upper-case? (string-ref s 0)))

(define (add-end-underscore s)
  (string-append s "_"))

(define (add-start-underscore s)
  (string-append "_" s))

(define (if-upper-underscore s)
  (if (start-with-uppercase? s)
      (add-end-underscore s)
      s))

(define (join-with-underscore s-list)
  (string-join s-list "_"))

(define (join-with-dot s-list)
  (string-join s-list "."))

(define (add-extension s)
  (string-append s ".glif"))


(define (purge-reserved-names s)
  (define (aux reserved)
    (if (null? reserved) s
        (if (string-ci=? (car reserved) s)
            (add-start-underscore s)
            (aux (cdr reserved)))))
  (aux  '("CON" "PRN" "AUX" "CLOCK$" "NUL" "A:-Z:" "COM1" 
                "LPT1" "LPT2" "LPT3" "COM2" "COM3" "COM4")))
   
(define (underscore-after-uppercase s)
  (let ([charlist (string->list s)])
    (string-join
     (map (lambda (c) 
            (if (char-upper-case? c)
                (list->string (list c #\_))
                (string c)))
         charlist)
     "")))

(define (underscore-after-nonlower s)
  (let ([charlist (string->list s)])
    (string-join
     (map (lambda (c) 
            (if (not (char-lower-case? c))
                (list->string (list c #\_))
                (string c)))
         charlist)
     "")))

(define (name-exists? name existing-names)
  (member (string-downcase name)
          (map string-downcase existing-names)))

(define (name->filename s [prefix ""] [suffix ".glif"] [existing-names '()])
  (let* ([max-length (- 255 (+ (string-length prefix)
                              (string-length suffix)))]
         [n (trim-to-max-length 
             (purge-initial-dot 
              (= (string-length prefix) 0)
              (underscore-after-uppercase 
               (purge-illegal-char s)))
             max-length)]
         [name 
           (join-with-dot 
           (map purge-reserved-names 
                (break-at-dot n)))]
         [fullname (string-append prefix name suffix)])
    (if (name-exists? fullname existing-names)
        (handle-name-clash name prefix suffix existing-names)
        name)))

(define (handle-name-clash name prefix suffix existing-names)
  (let ([max-length (- 255 (+ (string-length prefix)
                              (string-length suffix)
                              15))])
    (define (aux2 counter)
      (let ([s (number->string counter)])
        (if (> (string-length s) max-length)
            (error "Can't find a unique name")
            (let ([fullname (string-append prefix (number->string counter) suffix)])
              (if (name-exists? fullname existing-names)
                  (aux2 (+ counter 1))
                  fullname)))))
  (define (aux trimmed counter)
    (if (= counter 10) (aux2 1)
        (let* ([fullname (string-append prefix
                                        trimmed 
                                        (make-string 15 (string-ref 
                                                         (number->string counter)
                                                         0))
                                        suffix)])
          (if (name-exists? fullname existing-names)
              (aux trimmed (+ counter 1))
              fullname))))
 
         (aux (trim-to-max-length name max-length) 0)))
    
         



(define (namesymbol->filename s [prefix ""] [suffix ".glif"] [existing-names '()])
  (name->filename (symbol->string s)
                  prefix suffix existing-names))

         
(define (name->filename-ufo-2 s)
  (let* ([n (purge-initial-dot s)]
         [parts (break-at-dot n)]
         [first-parts (break-at-underscore (car parts))])
    (add-extension 
     (join-with-dot
      (cons (join-with-underscore
             (map if-upper-underscore first-parts))
            (cdr parts))))))