#lang racket

(require "plists.rkt"
         "glif.rkt"
         "names.rkt")

(provide (struct-out ufo:font)
         ufo:layer-name
         ufo:layer-glyphs
         ufo:get-layer
         ufo:map-layers
         ufo:for-each-layer
         ufo:filter-layer
         ufo:get-glyph
         ufo:get-layers-glyph
         ufo:map-glyphs
         ufo:for-each-glyph
         ufo:read-ufo
         ufo:write-ufo
         ufo3->ufo2
         ufo2->ufo3)


(struct ufo:font 
  (format creator fontinfo groups kerning features layers lib data images)
  #:transparent)

(define (ufo:layer-name layer)
  (car layer))

(define (ufo:layer-glyphs layer)
  (cdr layer))

(define (ufo:get-layer font [layer 'public.default])
  (assoc layer (ufo:font-layers font)))

(define (ufo:map-layers proc font)
  (map proc (ufo:font-layers font)))

(define (ufo:for-each-layer proc font)
  (for-each proc (ufo:font-layers font)))

(define (ufo:filter-layer proc layer)
  (cons (ufo:layer-name layer) (filter proc (ufo:layer-glyphs layer))))

(define (ufo:get-glyph font glyph [layer 'public.default])
  (let ([l (ufo:get-layer font layer)])
    (if l
        (dict-ref (ufo:layer-glyphs l) glyph #f)
        #f)))

(define (ufo:get-layers-glyph font glyph)
  (ufo:map-layers 
   (lambda (l) 
     (let ([name (ufo:layer-name l)])
       (cons name (ufo:get-glyph font glyph name))))
   font))
  
  
(define (ufo:map-glyphs proc font [layer 'public.default])
  (let ([l (ufo:get-layer font layer)])
    (if l
        (map (lambda (g) (proc (cdr g)))
             (ufo:layer-glyphs l))
        #f)))

(define (ufo:for-each-glyph proc font [layer 'public.default])
  (let ([l (ufo:get-layer font layer)])
    (if l
        (for-each (lambda (g) (proc (cdr g)))
             (ufo:layer-glyphs l))
        #f)))


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

  

(define (ufo:read-ufo path #:proc-data [proc-data #f] #:proc-images [proc-images #f])
  (if (directory-exists? path)
      (let* ([reader (ufo:reader path proc-data proc-images)]
             [meta ((reader 'meta))]
             [format (dict-ref meta 'formatVersion)]
             [creator (dict-ref meta 'creator)])
        (cond [(= format 2) (read-ufo2 creator reader)]
              [(= format 3) (read-ufo3 creator reader)]
              [#t (error "I can only read ufo 2 and ufo 3 fonts")]))
      (error "file do not exists")))
        
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
  (ufo:font 3 creator
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
          (copy-directory/files dir path))))
  (define (write-on-text-file text path)
    (when text 
      (call-with-output-file path 
        (lambda (o)
          (write-string text o)))))
  (define (get-layers-names)
    (letrec ([aux (lambda (acc layers names)
                    (match layers
                      [(list) acc]
                      [(list-rest (list-rest 'public.default _) rest-layers)
                       (aux (cons (cons 'public.default "glyphs") acc)
                            rest-layers
                            (cons "glyph" names))]
                      [(list-rest (list-rest l _) rest-layers)
                       (let ([name (namesymbol->filename l "glyphs." "" names)])
                                (aux (cons (cons l name) acc)
                                     rest-layers
                                     (cons name names)))]))])                
      (reverse (aux '() (ufo:font-layers font) '()))))
  
  (define layers-names (get-layers-names))
  (define (write-glyphs glyphs glyphsdir)
    (letrec ([aux (lambda (glyphs acc names)
                    (match glyphs
                      [(list) (make-hash (reverse acc))]
                      [(list-rest (list-rest n g) rest-glyphs)
                       (let ([name (namesymbol->filename n "" ".glif" names)])
                          (begin
                            (write-glif-file g (build-path glyphsdir name))
                            (aux rest-glyphs 
                                 (cons (cons (ufo:glyph-name g) name) acc)
                                 (cons name names))))]))])
                      

      (write-on-plist (aux  glyphs '() '())
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
     (map (lambda (layer) (list (symbol->string (car layer)) (cdr layer)))
          layers-names)
     (make-ufo-path "layercontents.plist")))
  
  
                  
      
  (let ([s (list 
            (cons 'meta (lambda () 
                          (write-on-plist (hash 'creator (ufo:font-creator font)
                                                'formatVersion (ufo:font-format font))
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
            (cons 'layercontents write-layercontents)
            (cons 'data (lambda () (write-directory (ufo:font-data font) (make-ufo-path "data") proc-data)))
            (cons 'images (lambda () (write-directory (ufo:font-images font) (make-ufo-path "images") proc-images))))])
    (lambda (k) (dict-ref s k))))

(define (ufo:write-ufo font path #:overwrite [overwrite #f] #:proc-data [proc-data #f] #:proc-images [proc-images #f])
  (let ([format (ufo:font-format font)]
        [writer (ufo:writer font path proc-data proc-images)])
    (if (and (directory-exists? path) (not overwrite))
        #f
        (begin
          (when (directory-exists? path)
            (delete-directory/files path))
          (make-directory path)
          (cond [(= format 2) (write-ufo2 writer)]
                [(= format 3) (write-ufo3 writer)]
                [#t (error "I can only write Ufo 2 and Ufo 3 files")])))))

(define (write-ufo2 writer)
  (begin
    ((writer 'meta))
    ((writer 'info))
    ((writer 'groups))
    ((writer 'kerning))
    ((writer 'features))
    ((writer 'layers))
    ((writer 'lib))))

(define (write-ufo3 writer)
  (begin
    ((writer 'meta))
    ((writer 'info))
    ((writer 'groups))
    ((writer 'kerning))
    ((writer 'features))
    ((writer 'layers))
    ((writer 'lib))
    ((writer 'layercontents))
    ((writer 'data))
    ((writer 'images))))


                 

