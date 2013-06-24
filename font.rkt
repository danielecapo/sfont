#lang racket

(require "plists.rkt"
         "glif.rkt"
         "names.rkt"
         "fontpict.rkt"
         "bezier.rkt"
         "vec.rkt"
         slideshow/pict-convert)

(provide (struct-out ufo:font)
         (struct-out ufo:layer)
         ufo:layer-name
         ufo:layer-info
         ufo:layer-glyphs
         ufo:get-layer
         set-layer
         ufo:map-layers
         ufo:for-each-layer
         ufo:filter-layer
         ufo:remove-glyph
         ufo:insert-glyph
         ufo:get-glyph
         ufo:get-layers-glyph
         ufo:map-glyphs
         ufo:for-each-glyph
         ufo:glyphs-in-font
         ufo:sort-glyphs
         ufo:read-ufo
         ufo:write-ufo
         ufo3->ufo2
         ufo2->ufo3
         ufo:decompose-glyph
         ufo:glyph-bounding-box
         ufo:font-bounding-box
         ufo:sidebearings
         ufo:intersections-at
         ufo:sidebearings-at
         ufo:glyph-signed-area
         ufo:set-sidebearings
         ufo:adjust-sidebearings)


(struct ufo:font 
  (format creator fontinfo groups kerning features layers lib data images)
  #:transparent
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (ufo:font-fontinfo f) 'ascender 750)]
          [descender (dict-ref (ufo:font-fontinfo f) 'descender -250)]
          [glyphs (ufo:map-glyphs draw-glyph  f)])
      (apply pictf:font ascender descender glyphs))))

(struct ufo:layer (name info glyphs) #:transparent)


(define (ufo:get-layer font [layer 'public.default])
  (findf (lambda (l) (eq? (ufo:layer-name l) layer))
         (ufo:font-layers font)))

(define (ufo:map-layers proc font)
  (map proc (ufo:font-layers font)))

(define (ufo:for-each-layer proc font)
  (for-each proc (ufo:font-layers font)))

(define (ufo:filter-layer proc layer)
  (filter proc (ufo:layer-glyphs layer)))

(define (ufo:get-glyph font glyph [layer 'public.default])
  (let ([l (ufo:get-layer font layer)])
    (if l
        (findf (lambda (g) (eq? (ufo:glyph-name g) glyph))
               (ufo:layer-glyphs l))
        #f)))

(define (set-layer f new-layer)
  (let ((layers (ufo:font-layers f))
        (new-name (ufo:layer-name new-layer)))
    (struct-copy ufo:font f
                 [layers
                  (dict-values
                   (dict-set (ufo:map-layers 
                              (lambda (l) (cons (ufo:layer-name l) l)) f)
                             new-name new-layer))])))
    
    
(define (glyphs->hash layer)
  (make-immutable-hash (map (lambda (g) (cons (ufo:glyph-name g) g))
                            (ufo:layer-glyphs layer))))

(define (hash->glyphs gh)
  (hash-values gh))



(define (ufo:remove-glyph f glyph [layername 'public.default])
  (let ((l (ufo:get-layer f layername)))
    (set-layer f (struct-copy ufo:layer l 
                              [glyphs (hash->glyphs 
                                       (hash-remove (glyphs->hash l) 
                                                    glyph))]))))
    

(define (ufo:insert-glyph f glyph [layername 'public.default])
  (let ((l (ufo:get-layer f layername)))
    (set-layer f (struct-copy ufo:layer l 
                              [glyphs (hash->glyphs 
                                       (hash-set (glyphs->hash l)
                                                 (ufo:glyph-name glyph)                                                              
                                                 glyph))]))))
                     
  

(define (ufo:get-layers-glyph font glyph)
  (ufo:map-layers 
   (lambda (l) 
     (let ([name (ufo:layer-name l)])
       (ufo:get-glyph font glyph name)))
   font))
  
  
(define (ufo:map-glyphs proc font [layer 'public.default])
  (let ([l (ufo:get-layer font layer)])
    (if l
        (map (lambda (g) (proc g))
             (ufo:layer-glyphs l))
        (error "Layer does not exist"))))

(define (ufo:for-each-glyph proc font [layer 'public.default])
  (let ([l (ufo:get-layer font layer)])
    (if l
        (for-each (lambda (g) (proc g))
             (ufo:layer-glyphs l))
        (error "Layer does not exist"))))


(define (ufo:glyphs-in-font f)
  (set->list
    (foldl set-union
           (set)
           (ufo:map-layers (lambda (l) 
                             (list->set (map ufo:glyph-name (ufo:layer-glyphs l))))
                           f))))

(define (ufo:sort-glyphs f)
  (struct-copy ufo:font f 
               [layers (ufo:map-layers 
                        (lambda (l) 
                          (struct-copy ufo:layer l
                                       [glyphs (sort (ufo:layer-glyphs l)
                                                     #:key ufo:glyph-name
                                                     (lambda (a b) (string<? (symbol->string a)
                                                                             (symbol->string b))))]))
                        f)]))
        


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
  (define (read-groups)
    (make-immutable-hash
     (hash-map (read-from-plist (make-ufo-path "groups.plist"))
               (lambda (name content)
                 (cons name (map string->symbol content))))))
  (define (read-layerinfo glyphsdir)
    (read-from-plist 
     (build-path (make-ufo-path glyphsdir) "layerinfo.plist")))
  (define (read-layers)
    (let ([layers (read-from-plist (make-ufo-path "layercontents.plist"))])
      (map (lambda (layer) 
             (ufo:layer (string->symbol (car layer))
                        (read-layerinfo (cadr layer)) 
                        (read-glyphs (cadr layer))))
           (if layers layers (list (list "public.default" "glyphs"))))))
          
  (define (read-glyphs glyphsdir)
    (let* ([glyphspath (make-ufo-path glyphsdir)]
           [contents (read-from-plist (build-path glyphspath "contents.plist"))])
      (hash-map contents
                (lambda (k v) (read-glif-file (build-path glyphspath v) k)))))
           
  (define (read-from-directory path [proc #f])
    (if proc
        (proc path)
        path))
  (let ([s (list 
            (cons 'meta (lambda () (read-from-plist (make-ufo-path "metainfo.plist"))))
            (cons 'info (lambda () (read-from-plist (make-ufo-path "fontinfo.plist"))))
            (cons 'groups read-groups) 
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
               [layers (list (ufo:layer 'public.default #f 
                                        (map glyph2->glyph1
                                             (ufo:layer-glyphs (ufo:get-layer font 'public.default)))))]))
            
(define (ufo2->ufo3 font) 
  (struct-copy ufo:font font [format 3]
               [layers (ufo:map-layers
                        (lambda (l)
                          (struct-copy ufo:layer l
                                       [glyphs (map glyph1->glyph2 
                                                    (ufo:layer-glyphs l))]))
                        font)]))
                               
               

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
  (define (write-groups)
    (write-on-plist (make-immutable-hash
                     (hash-map (ufo:font-groups font)
                               (lambda (name content)
                                 (cons name (map symbol->string content)))))
                    (make-ufo-path "groups.plist")))
  (define (get-layers-names)
    (letrec ([aux (lambda (acc layers names)
                    (match layers
                      [(list) acc]
                      [(list-rest (ufo:layer 'public.default _ _) rest-layers)
                       (aux (cons (cons 'public.default "glyphs") acc)
                            rest-layers
                            (cons "glyphs" names))]
                      [(list-rest (ufo:layer l _ _) rest-layers)
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
                      [(list-rest g rest-glyphs)
                       (let ([name (namesymbol->filename (ufo:glyph-name g) "" ".glif" names)])
                          (begin
                            (write-glif-file g (build-path glyphsdir name))
                            (aux rest-glyphs 
                                 (cons (cons (ufo:glyph-name g) name) acc)
                                 (cons name names))))]))])
                      

      (write-on-plist (aux  glyphs '() '())
                      (build-path glyphsdir "contents.plist"))))
      
  (define (write-layers)
    (let ((layers-hash (make-immutable-hash 
                        (map (lambda (l) (cons (ufo:layer-name l) l))
                            (ufo:font-layers font)))))
      (for-each (lambda (layer)
                  (begin
                    (let ([dir (make-ufo-path (cdr layer))]
                          [l (dict-ref layers-hash (car layer))])
                      (make-directory dir)
                      (write-glyphs (ufo:layer-glyphs l) dir)
                      (write-layerinfo (ufo:layer-info l) dir))))
                layers-names)))
  
  (define (write-layerinfo info dir)
    (write-on-plist info (build-path (make-ufo-path dir) "layerinfo.plist")))
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
            (cons 'groups write-groups)
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

; ufo:decompose-glyph
; ufo:font, GlyphName -> ufo:glyph
; decompose glyph components to outlines

(define (ufo:decompose-glyph f gn)
  (let* ([g (ufo:get-glyph f gn)]
         [cs (ufo:glyph-components g)]
         [bases (map (lambda (c) (ufo:get-glyph f (ufo:component-base c))) cs)]
         [dcs (apply append (map ufo:component->outlines cs bases))])
    (struct-copy ufo:glyph g
                 [components null]
                 [contours (append (ufo:glyph-contours g) dcs)])))
                     
; ufo:glyph-bounding-box
; ufo:font, GlyphName -> BoundingBox
; produces the Bounding Box for the given glyph

(define (ufo:glyph-bounding-box f gn)
  (let* ([g (ufo:decompose-glyph f gn)]
         [cs (ufo:glyph-contours g)])
    (if (null? cs)
        #f
        (apply combine-bounding-boxes 
               (map (lambda (c) 
                      (bezier-bounding-box (contour->bezier c)))
                    cs)))))

; ufo:font-bounding-box
; ufo:font -> BoundingBox
; produces the Bounding Box for the given font

(define (ufo:font-bounding-box f)
  (apply combine-bounding-boxes 
         (filter identity
                 (ufo:map-glyphs (lambda (g) 
                                   (ufo:glyph-bounding-box f (ufo:glyph-name g)))
                                 f))))

; ufo:sidebearings 
; ufo:font, GlyphName -> (Number . Number)
; produces a pair representing the left and right sidebearings for the give glyph

(define (ufo:sidebearings f gn)
  (let* ([g (ufo:decompose-glyph f gn)]
         [bb (ufo:glyph-bounding-box f gn)]
         [a (ufo:advance-width (ufo:glyph-advance g))])
    (if bb
        (cons (vec-x (car bb))
              (- a (vec-x (cdr bb))))
        #f)))

; ufo:intersections-at 
; ufo:font, GlyphName, Number -> List of Vec
; produces a list of the intersections of outlines with the line y = h

(define (ufo:intersections-at f gn h)
  (let* ([g (ufo:decompose-glyph f gn)]
         [cs (ufo:glyph-contours g)])
    (sort 
     (remove-duplicates
      (apply append 
             (map (lambda (c) 
                    (bezier-intersect-hor h (contour->bezier c)))
                  cs))
      vec=)
     < #:key vec-x)))

; ufo:sidebearings-at 
; ufo:font, GlyphName, Number -> (Number . Number)
; produces a pair representing sidebearings measured at y = h

(define (ufo:sidebearings-at f gn h)
  (let* ([g (ufo:decompose-glyph f gn)]
         [is (ufo:intersections-at f gn h)]
         [a (ufo:advance-width (ufo:glyph-advance g))])
    (cons (vec-x (car is)) (- a (vec-x (last is))))))
    
  
; ufo:glyph-signed-area
; ufo:font, GlyphName -> Number
; produces the area for the given glyph (negative if in the wrong direction)

(define (ufo:glyph-signed-area f gn)
  (let* ([g (ufo:decompose-glyph f gn)]
         [cs (ufo:glyph-contours g)])
    (foldl + 0 
           (map (lambda (c) 
                  (bezier-signed-area (contour->bezier c)))
                cs))))

; ufo:set-sidebearings
; ufo:font, GlyphName, Number, Number -> ufo:glyph
; set left and right sidebearings for the glyph named gn

(define (ufo:set-sidebearings f gn left right)
  (let* ([g (ufo:get-glyph f gn)]
         [os (ufo:sidebearings f gn)]
         [oa (ufo:advance-width (ufo:glyph-advance g))])     
    (if os
        (let* ([la (- left (car os))]
               [ra (+ la (- right (cdr os)))])
          (struct-copy ufo:glyph 
                       (translate g (vec la 0))
                       [advance (ufo:advance (+ oa ra)
                                             (ufo:advance-height 
                                              (ufo:glyph-advance g)))]))
        #f)))
                       
         
; ufo:adjust-sidebearings
; ufo:font, GlyphName, Number, Number -> ufo:glyph
; adjust left and right sidebearings for the glyph named gn

(define (ufo:adjust-sidebearings f gn left right)
  (let* ([g (ufo:get-glyph f gn)]
         [os (ufo:sidebearings f gn)])     
    (if os
        (ufo:set-sidebearings f gn (+ (car os) left) 
                              (+ (cdr os) right))
        #f)))
        

                 


