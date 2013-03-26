#lang racket

(require "plists.rkt"
         "glif.rkt"
         "names.rkt")


(struct ufo:font 
  (format creator fontinfo groups kerning features layers lib data images)
  #:transparent)



(define (ufo:reader path [proc-data #f] [proc-images #f])
  (define (make-ufo-path file)
    (build-path path file))
  (define (read-from-plist path)
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (if (file-exists? path)
          (read-dict path)
          #f)))
  (define (read-from-text-file path)
    (if (file-exists? path)
         (call-with-input-file path port->string)
         #f))
  (define (read-layers)
    (let ([layers (read-from-plist (make-ufo-path "layercontents.plist"))])
      (if layers
          (map (lambda (layer) 
                 (cons (string->symbol (car layer)) (read-glyphs (cadr layer))))
               layers)
          (list (cons 'public.default (read-glyphs "glyphs"))))))
  (define (read-glyphs glyphsdir)
    (let* ([glyphspath (make-ufo-path glyphsdir)]
           [contents (read-from-plist (build-path glyphspath "contents.plist"))])
      (hash-map contents
                (lambda (k v) (cons k
                             (read-glif-file (build-path glyphspath v)))))))
           
  (define (read-from-directory path [proc #f])
    (if proc
        (proc path)
        path))
  (let ([s (list 
            (cons 'meta (lambda () (read-from-plist (make-ufo-path "metainfo.plist"))))
            (cons 'info (lambda () (read-from-plist (make-ufo-path "fontinfo.plist"))))
            (cons 'groups (lambda () (read-from-plist (make-ufo-path "groups.plist"))))
            (cons 'kerning (lambda () (read-from-plist (make-ufo-path "kerning.plist"))))
            (cons 'features (lambda () (read-from-text-file (make-ufo-path "features.fea"))))
            (cons 'lib (lambda () (read-from-plist (make-ufo-path "lib.plist"))))
            (cons 'layers read-layers)
            (cons 'data (lambda () (read-from-directory (make-ufo-path "data") proc-data)))
            (cons 'images (lambda () (read-from-directory (make-ufo-path "images") proc-images))))])
    (lambda (k) (dict-ref s k))))

  

(define (ufo:read-ufo path [proc-data #f] [proc-images #f])
  (let* ([reader (ufo:reader path proc-data proc-images)]
         [meta ((reader 'meta))]
         [format (dict-ref meta 'formatVersion)]
         [creator (dict-ref meta 'creator)])
    (cond [(= format 2) (read-ufo2 creator reader)]
          [(= format 3) (read-ufo3 creator reader)]
          [#t (error "I can only read ufo 2 and ufo 3 fonts")])))
        
(define (read-ufo2 creator reader)
  (ufo:font 2 creator
            ((reader 'info))
            ((reader 'groups))
            ((reader 'kerning))
            ((reader 'features))
            ((reader 'layers))
            ((reader 'lib))
            #f
            #f))

(define (read-ufo3 creator reader)
  (ufo:font 2 creator
            ((reader 'info))
            ((reader 'groups))
            ((reader 'kerning))
            ((reader 'features))
            ((reader 'layers))
            ((reader 'lib))
            ((reader 'data))
            ((reader 'images))))

(define (ufo3->ufo2 font)
  (struct-copy ufo:font font [format 2] [data #f] [images #f]
               [layers (list (cons 'public.default) 
                             (map glyph2->glyph1
                                  (dict-ref (ufo:font-layers) 'public-default)))]))
            
(define (ufo2->ufo3 font) (struct-copy ufo:font font [format 3]))

(define (ufo:writer font path [proc-data #f] [proc-images #f])
  (define (make-ufo-path file)
    (build-path path file))
  (define (write-on-plist dict path)
    (when dict 
      (write-dict dict path)))
  (define (write-directory dir path [proc #f])
    (when dir
      (if proc
          (proc path)
          (copy-directory/files path dir))))
  (define (write-on-text-file text path)
    (when text 
      (call-with-output-file path 
        (lambda (o)
          (write-string text o)))))
  
  (define (get-layers-names)
    (letrec ([aux (lambda (acc layers names)
                    (if (null? layers)
                        acc
                        (let ([l (car layers)])
                          (if (equal? (car l) 'public.default)
                              (aux (cons (cons 'public.default "glyphs") acc)
                                   (cdr layers)
                                   (cons "glyphs" names))
                              (let ([name (namesymbol->filename (car l) "glyphs." "" names)])
                                (aux (cons (cons (car l) name) acc)
                                     (cdr layers)
                                     (cons name names)))))))])
      (reverse (aux '() (ufo:font-layers font) '()))))
  
  (define layers-names (get-layers-names))
  (define (write-glyphs glyphs glyphsdir)
    (letrec ([aux (lambda (glyphs acc names)
                    (if (null? glyphs)
                        (make-hash (reverse acc))
                        (let ([name (namesymbol->filename (ufo:glyph-name (car glyphs))
                                                          "" ".glif" names)])
                          (begin
                            (write-glif-file (car glyphs) (build-path glyphsdir name))
                            (aux (cdr glyphs) 
                                 (cons (cons (ufo:glyph-name (car glyphs)) name) acc)
                                 (cons name names))))))])
                            
      (write-on-plist (aux '() glyphs '())
                      (build-path glyphsdir "contents.plist"))))
  (define (write-layers)
    (for-each (lambda (layer)
                (begin
                  (let ([dir (make-ufo-path (cdr layer))])
                    (make-directory dir)
                    (write-glyphs (dict-ref (ufo:font-layers font)
                                            (car layer))
                                  dir))))
              layers-names))
  (define (write-layercontents)
    (write-on-plist 
     (map (lambda (layer) (list (car layer) (cdr layer)))
          layers-names)
     (make-ufo-path "layercontents.plist")))
  
  
                  
      
  (let ([s (list 
            (cons 'meta (lambda () 
                          (write-on-plist (list (cons 'creator (ufo:font-creator font))
                                                (cons 'formatVersion (ufo:font-format font)))
                                          (make-ufo-path "metainfo.plist"))))
            (cons 'info (lambda () 
                          (write-on-plist (ufo:font-fontinfo font) 
                                          (make-ufo-path "fontinfo.plist"))))
            (cons 'groups (lambda () (write-on-plist (ufo:font-groups font) 
                                                     (make-ufo-path "groups.plist"))))
            (cons 'kerning (lambda () (write-on-plist (ufo:font-kerning font) 
                                                      (make-ufo-path "kerning.plist"))))
            (cons 'features (lambda () (write-on-text-file (ufo:font-features font)
                                                           (make-ufo-path "features.fea"))))
            (cons 'lib (lambda () (write-on-plist (ufo:font-lib font) 
                                                  (make-ufo-path "lib.plist"))))
            (cons 'layers write-layers)
            (cons 'layrecontents write-layercontents)
            (cons 'data (lambda () (write-directory (ufo:font-data font) (make-ufo-path "data") proc-data)))
            (cons 'images (lambda () (write-directory (ufo:font-images font) (make-ufo-path "images") proc-images))))])
    (lambda (k) (dict-ref s k))))

(define (ufo:write-ufo font path [proc-data #f] [proc-images #f])
  (let ([format (ufo:font-format font)]
        [writer (ufo:writer font path proc-data proc-images)])
    (cond ([(= format 2) (write-ufo2 writer)]
           [(= format 3) (write-ufo3 writer)]
           [#t (error "I can only write Ufo 2 and Ufo 3 files")]))))

(define (write-ufo2 writer)
  ((writer 'meta)
  ((writer 'info))
  ((writer 'groups))
  ((writer 'kerning))
  ((writer 'features))
  ((writer 'layers))
  ((writer 'lib))))

(define (write-ufo3 writer)
  ((writer 'meta)
  ((writer 'info))
  ((writer 'groups))
  ((writer 'kerning))
  ((writer 'features))
  ((writer 'layers))
  ((writer 'lib)))
  ((writer 'layercontents))
  ((writer 'data))
  ((writer 'images)))


                 
(define (read-meta ufo-path)
  (read-dict (build-path ufo-path "metainfo.plist")))
    
(define (read-info ufo-path)
  (read-dict (build-path ufo-path "fontinfo.plist")))

(define (read-groups ufo-path)
  (read-dict (build-path ufo-path "groups.plist")))

(define (read-kerning ufo-path)
  (read-dict (build-path ufo-path "kerning.plist")))

(define (read-lib ufo-path)
  (read-dict (build-path ufo-path "lib.plist")))

(define (read-layercontents ufo-path)
  (read-dict (build-path ufo-path "layercontents.plist")))

(define (read-features ufo-path)
  (call-with-input-file (build-path ufo-path "features.fea")
    port->string))


(define (read-data ufo-path [proc #f])
  (if proc
      (proc (build-path ufo-path "data"))
      (build-path ufo-path "data")))

(define (read-images ufo-path [proc #f])
  (if proc
      (proc (build-path ufo-path "images"))
      (build-path ufo-path "images")))



;(struct ufo:group
;  (name glyphs)
;  #:transparent)
;
;(struct ufo:kerning
;  (name adjustments)
;  #:transparent)
;
;(define (ufo:adjust-name adj)
;  (car adj))
;
;(define (ufo:adjust-amount adj)
;  (cdr adj))
;
;(struct ufo:layer 
;  (name glyphs)
;  #:transparent)
;
;(define-syntax-rule 
;  (ufo:make-kerning first [second value] ...)
;  (ufo:kerning first 
;               (list (cons second value) ...)))

  
  