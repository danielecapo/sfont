#lang racket

(require "plists.rkt"
         "glif.rkt"
         "names.rkt"
         "fontpict.rkt"
         "bezier.rkt"
         "vec.rkt"
         slideshow/pict-convert)

(provide (struct-out font)
         (struct-out layer)
         layer-name
         layer-info
         layer-glyphs
         get-layer
         set-layer
         map-layers
         for-each-layer
         filter-layer
         remove-glyph
         insert-glyph
         get-glyph
         get-layers-glyph
         map-glyphs
         for-each-glyph
         glyphs-in-font
         sort-glyphs
         read-ufo
         write-ufo
         ufo3->ufo2
         ufo2->ufo3
         decompose-glyph
         decompose-layer
         glyph-bounding-box
         font-bounding-box
         sidebearings
         intersections-at
         sidebearings-at
         glyph-signed-area
         set-sidebearings
         adjust-sidebearings
         correct-directions)


(struct font 
  (format creator fontinfo groups kerning features layers lib data images)
  #:transparent
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (font-fontinfo f) 'ascender 750)]
          [descender (dict-ref (font-fontinfo f) 'descender -250)]
          [glyphs (map-glyphs draw-glyph  f)])
      (apply pictf:font ascender descender glyphs))))

(struct layer (name info glyphs) #:transparent)


(define (get-layer font [layer 'public.default])
  (findf (lambda (l) (eq? (layer-name l) layer))
         (font-layers font)))

(define (map-layers proc font)
  (map proc (font-layers font)))

(define (for-each-layer proc font)
  (for-each proc (font-layers font)))

(define (filter-layer proc layer)
  (filter proc (layer-glyphs layer)))

(define (get-glyph font glyph [layer 'public.default])
  (let ([l (get-layer font layer)])
    (if l
        (findf (lambda (g) (eq? (glyph-name g) glyph))
               (layer-glyphs l))
        #f)))

(define (set-layer f new-layer)
  (let ((layers (font-layers f))
        (new-name (layer-name new-layer)))
    (struct-copy font f
                 [layers
                  (dict-values
                   (dict-set (map-layers 
                              (lambda (l) (cons (layer-name l) l)) f)
                             new-name new-layer))])))
    
    
(define (glyphs->hash layer)
  (make-immutable-hash (map (lambda (g) (cons (glyph-name g) g))
                            (layer-glyphs layer))))

(define (hash->glyphs gh)
  (hash-values gh))



(define (remove-glyph f glyph [layername 'public.default])
  (let ((l (get-layer f layername)))
    (set-layer f (struct-copy layer l 
                              [glyphs (hash->glyphs 
                                       (hash-remove (glyphs->hash l) 
                                                    glyph))]))))
    

(define (insert-glyph f glyph [layername 'public.default])
  (let ((l (get-layer f layername)))
    (set-layer f (struct-copy layer l 
                              [glyphs (hash->glyphs 
                                       (hash-set (glyphs->hash l)
                                                 (glyph-name glyph)                                                              
                                                 glyph))]))))
                     
  

(define (get-layers-glyph font glyph)
  (map-layers 
   (lambda (l) 
     (let ([name (layer-name l)])
       (get-glyph font glyph name)))
   font))
  
  
(define (map-glyphs proc font [layer 'public.default])
  (let ([l (get-layer font layer)])
    (if l
        (map (lambda (g) (proc g))
             (layer-glyphs l))
        (error "Layer does not exist"))))

(define (for-each-glyph proc font [layer 'public.default])
  (let ([l (get-layer font layer)])
    (if l
        (for-each (lambda (g) (proc g))
             (layer-glyphs l))
        (error "Layer does not exist"))))


(define (glyphs-in-font f)
  (set->list
    (foldl set-union
           (set)
           (map-layers (lambda (l) 
                             (list->set (map glyph-name (layer-glyphs l))))
                           f))))

(define (sort-glyphs f)
  (struct-copy font f 
               [layers (map-layers 
                        (lambda (l) 
                          (struct-copy layer l
                                       [glyphs (sort (layer-glyphs l)
                                                     #:key glyph-name
                                                     (lambda (a b) (string<? (symbol->string a)
                                                                             (symbol->string b))))]))
                        f)]))
        


(define (reader path [proc-data #f] [proc-images #f])
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
    (let ([g (read-from-plist (make-ufo-path "groups.plist"))])
      (if g
          (make-immutable-hash
           (hash-map g (lambda (name content)
                         (cons name (map string->symbol content)))))
          #f)))
  (define (read-layerinfo glyphsdir)
    (read-from-plist 
     (build-path (make-ufo-path glyphsdir) "layerinfo.plist")))
  (define (read-layers)
    (let ([layers (read-from-plist (make-ufo-path "layercontents.plist"))])
      (map (lambda (l) 
             (layer (string->symbol (car l))
                        (read-layerinfo (cadr l)) 
                        (read-glyphs (cadr l))))
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

  

(define (read-ufo path #:proc-data [proc-data #f] #:proc-images [proc-images #f])
  (if (directory-exists? path)
      (let* ([reader (reader path proc-data proc-images)]
             [meta ((reader 'meta))]
             [format (dict-ref meta 'formatVersion)]
             [creator (dict-ref meta 'creator)])
        (cond [(= format 2) (read-ufo2 creator reader)]
              [(= format 3) (read-ufo3 creator reader)]
              [#t (error "I can only read ufo 2 and ufo 3 fonts")]))
      (error "file do not exists")))
        
(define (read-ufo2 creator reader)
  (font 2 creator
            ((reader 'info))
            ((reader 'groups))
            ((reader 'kerning))
            ((reader 'features))
            ((reader 'layers))
            ((reader 'lib))
            #f
            #f))

(define (read-ufo3 creator reader)
  (font 3 creator
            ((reader 'info))
            ((reader 'groups))
            ((reader 'kerning))
            ((reader 'features))
            ((reader 'layers))
            ((reader 'lib))
            ((reader 'data))
            ((reader 'images))))

(define (ufo3->ufo2 f)
  (struct-copy font f [format 2] [data #f] [images #f]
               [layers (list (layer 'public.default #f 
                                        (map glyph2->glyph1
                                             (layer-glyphs (get-layer f 'public.default)))))]))
            
(define (ufo2->ufo3 f) 
  (struct-copy font f [format 3]
               [layers (map-layers
                        (lambda (l)
                          (struct-copy layer l
                                       [glyphs (map glyph1->glyph2 
                                                    (layer-glyphs l))]))
                        f)]))
                               
               

(define (writer font path [proc-data #f] [proc-images #f])
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
                     (hash-map (font-groups font)
                               (lambda (name content)
                                 (cons name (map symbol->string content)))))
                    (make-ufo-path "groups.plist")))
  (define (get-layers-names)
    (letrec ([aux (lambda (acc layers names)
                    (match layers
                      [(list) acc]
                      [(list-rest (layer 'public.default _ _) rest-layers)
                       (aux (cons (cons 'public.default "glyphs") acc)
                            rest-layers
                            (cons "glyphs" names))]
                      [(list-rest (layer l _ _) rest-layers)
                       (let ([name (namesymbol->filename l "glyphs." "" names)])
                                (aux (cons (cons l name) acc)
                                     rest-layers
                                     (cons name names)))]))])                
      (reverse (aux '() (font-layers font) '()))))
  
  (define layers-names (get-layers-names))
  (define (write-glyphs glyphs glyphsdir)
    (letrec ([aux (lambda (glyphs acc names)
                    (match glyphs
                      [(list) (make-hash (reverse acc))]
                      [(list-rest g rest-glyphs)
                       (let ([name (namesymbol->filename (glyph-name g) "" ".glif" names)])
                          (begin
                            (write-glif-file g (build-path glyphsdir name))
                            (aux rest-glyphs 
                                 (cons (cons (glyph-name g) name) acc)
                                 (cons name names))))]))])
                      

      (write-on-plist (aux  glyphs '() '())
                      (build-path glyphsdir "contents.plist"))))
      
  (define (write-layers)
    (let ((layers-hash (make-immutable-hash 
                        (map (lambda (l) (cons (layer-name l) l))
                            (font-layers font)))))
      (for-each (lambda (l)
                  (begin
                    (let ([dir (make-ufo-path (cdr l))]
                          [la (dict-ref layers-hash (car l))])
                      (make-directory dir)
                      (write-glyphs (layer-glyphs la) dir)
                      (write-layerinfo (layer-info la) dir))))
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
                          (write-on-plist (hash 'creator (font-creator font)
                                                'formatVersion (font-format font))
                                          (make-ufo-path "metainfo.plist"))))
            (cons 'info (lambda () 
                          (write-on-plist (font-fontinfo font) 
                                          (make-ufo-path "fontinfo.plist"))))
            (cons 'groups write-groups)
            (cons 'kerning (lambda () (write-on-plist (font-kerning font) 
                                                      (make-ufo-path "kerning.plist"))))
            (cons 'features (lambda () (write-on-text-file (font-features font)
                                                           (make-ufo-path "features.fea"))))
            (cons 'lib (lambda () (write-on-plist (font-lib font) 
                                                  (make-ufo-path "lib.plist"))))
            (cons 'layers write-layers)
            (cons 'layercontents write-layercontents)
            (cons 'data (lambda () (write-directory (font-data font) (make-ufo-path "data") proc-data)))
            (cons 'images (lambda () (write-directory (font-images font) (make-ufo-path "images") proc-images))))])
    (lambda (k) (dict-ref s k))))

(define (write-ufo font path #:overwrite [overwrite #f] #:proc-data [proc-data #f] #:proc-images [proc-images #f])
  (let ([format (font-format font)]
        [writer (writer font path proc-data proc-images)])
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

; decompose-glyph
; font, GlyphName, LayerName -> glyph
; decompose glyph components to outlines

(define (decompose-glyph f gn [ln 'public.default])
  (let* ([g (get-glyph f gn ln)]
         [cs (glyph-components g)]
         [bases (map (lambda (c) (get-glyph f (component-base c) ln)) cs)]
         [dcs (apply append (map component->outlines cs bases))])
    (struct-copy glyph g
                 [components null]
                 [contours (append (glyph-contours g) dcs)])))

; decompose-layer
; font, LayerName -> layer
; produces a new layer with glyphs decomposed

(define (decompose-layer f [ln 'public.default])
  (let* ([l (get-layer f ln)]
         [gh (glyphs->hash l)]
         [dec (lambda (g)
                (let ([d (apply 
                              append
                              (map (lambda (c) 
                                     (component->outlines
                                      c (hash-ref gh (component-base c))))
                                   (glyph-components g)))])
                  (struct-copy glyph g [components null]
                               [contours (append (glyph-contours g) d)])))])
    (struct-copy layer l [glyphs (hash-map gh (lambda (k v) (dec v)))])))


                     
; glyph-bounding-box
; font, GlyphName -> BoundingBox
; produces the Bounding Box for the given glyph

(define (glyph-bounding-box f gn)
  (let* ([g (decompose-glyph f gn)]
         [cs (glyph-contours g)])
    (if (null? cs)
        #f
        (apply combine-bounding-boxes 
               (map (lambda (c) 
                      (bezier-bounding-box (contour->bezier c)))
                    cs)))))

; font-bounding-box
; font, Boolean -> BoundingBox
; produces the Bounding Box for the given font

(define (font-bounding-box f [components #t])
  (let ([gs (filter
             (lambda (g) (> (length (glyph-contours g)) 0))
             (if components (layer-glyphs (decompose-layer f))
                (layer-glyphs (get-layer f))))])
    (apply combine-bounding-boxes 
           (filter identity
                   (map (lambda (g) 
                          (apply combine-bounding-boxes
                                 (map (lambda (c)
                                        (bezier-bounding-box (contour->bezier c)))
                                      (glyph-contours g))))
                        gs)))))

; sidebearings 
; font, GlyphName -> (Number . Number)
; produces a pair representing the left and right sidebearings for the give glyph

(define (sidebearings f gn)
  (let* ([g (decompose-glyph f gn)]
         [bb (glyph-bounding-box f gn)]
         [a (advance-width (glyph-advance g))])
    (if bb
        (cons (vec-x (car bb))
              (- a (vec-x (cdr bb))))
        #f)))

; intersections-at 
; font, GlyphName, Number -> List of Vec
; produces a list of the intersections of outlines with the line y = h

(define (intersections-at f gn h)
  (let* ([g (decompose-glyph f gn)]
         [cs (glyph-contours g)])
    (sort 
     (remove-duplicates
      (apply append 
             (map (lambda (c) 
                    (bezier-intersect-hor h (contour->bezier c)))
                  cs))
      vec=)
     < #:key vec-x)))

; sidebearings-at 
; font, GlyphName, Number -> (Number . Number)
; produces a pair representing sidebearings measured at y = h

(define (sidebearings-at f gn h)
  (let* ([g (decompose-glyph f gn)]
         [is (intersections-at f gn h)]
         [a (advance-width (glyph-advance g))])
    (if (null? is)
        #f
        (cons (vec-x (car is)) (- a (vec-x (last is)))))))
    
  
; glyph-signed-area
; font, GlyphName -> Number
; produces the area for the given glyph (negative if in the wrong direction)

(define (glyph-signed-area f gn)
  (let* ([g (decompose-glyph f gn)]
         [cs (glyph-contours g)])
    (foldl + 0 
           (map (lambda (c) 
                  (bezier-signed-area (contour->bezier c)))
                cs))))

; set-sidebearings
; font, GlyphName, Number, Number -> glyph
; set left and right sidebearings for the glyph named gn

(define (set-sidebearings f gn left right)
  (let* ([g (get-glyph f gn)]
         [os (sidebearings f gn)]
         [oa (advance-width (glyph-advance g))])     
    (if os
        (let* ([la (- left (car os))]
               [ra (+ la (- right (cdr os)))])
          (struct-copy glyph 
                       (translate g (vec la 0))
                       [advance (advance (+ oa ra)
                                             (advance-height 
                                              (glyph-advance g)))]))
        #f)))
                       
     
; set-sidebearings-at
; font, GlyphName, Number, Number, Number -> glyph
; set left and right sidebearings (measured at y = h) for the glyph named gn 

(define (set-sidebearings-at f gn left right h)
  (let* ([g (get-glyph f gn)]
         [os (sidebearings-at f gn h)]
         [oa (advance-width (glyph-advance g))])     
    (if os
        (let* ([la (- left (car os))]
               [ra (+ la (- right (cdr os)))])
          (struct-copy glyph 
                       (translate g (vec la 0))
                       [advance (advance (+ oa ra)
                                             (advance-height 
                                              (glyph-advance g)))]))
        #f)))

; adjust-sidebearings
; font, GlyphName, Number, Number -> glyph
; adjust left and right sidebearings for the glyph named gn

(define (adjust-sidebearings f gn left right)
  (let* ([g (get-glyph f gn)]
         [os (sidebearings f gn)])     
    (if os
        (set-sidebearings f gn (+ (car os) left) 
                              (+ (cdr os) right))
        #f)))

; correct-directions
; font -> font
; produces a new font with contour in the correct direction

(define (correct-directions f)
  (struct-copy font f
               [layers 
                (map-layers 
                 (lambda (l)
                   (struct-copy layer l
                                [glyphs 
                                 (map-glyphs 
                                  glyph-correct-directions
                                  f (layer-name l))]))
                 f)]))

        

                 


