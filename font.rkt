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
         set-sidebearings-at
         adjust-sidebearings
         correct-directions
         print-glyph)

;;; DATA DEFINITIONS
;;; Font
;;; (font Number Symbol HashTable HashTable String ListOfLayers HashTables Any ListOfStrings)
;;; 
;;; Layer
;;; (layer Symbol HashTable HashTable)
;;; Layer can be build from a list of Glyphs or from an HashTable (Name . Glyph)

(struct font 
  (format creator fontinfo groups kerning features layers lib data images)
  #:transparent
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (font-fontinfo f) 'ascender 750)]
          [descender (dict-ref (font-fontinfo f) 'descender -250)]
          [glyphs (map (lambda (g) (draw-glyph (decompose-glyph f g)))
                       (get-glyphs f *text*))])
      (apply pictf:font ascender descender glyphs))))

(struct layer (name info glyphs) 
  #:transparent
  #:guard (lambda (name info glyphs tn)
            (values name
                    info
                    (if (hash? glyphs)
                        (if (immutable? glyphs)
                            glyphs
                            (make-immutable-hash (hash->list glyphs)))
                    (glyphlist->hashglyphs glyphs)))))

; glyphlist->hashglyphs
; ListOfGlyphs -> HashTableOfGlyphs
; produce an immutable hashtable where keys are the names of glyphs and values are the glyphs

(define (glyphlist->hashglyphs gs)
  (make-immutable-hash 
   (map (lambda (g) (cons (glyph-name g) g))
        gs)))

; hashglyphs->glyphlist
; HashTableOfGlyphs -> ListOfGlyphs
; produce a list of glyphs from hashtables of glyphs

(define (hashglyphs->glyphlist gh)
  (hash-values gh))


(define (get-layer f [layer 'public.default])
  (findf (lambda (l) (eq? (layer-name l) layer))
         (font-layers f)))

(define (map-layers proc f)
  (map proc (font-layers f)))

(define (for-each-layer proc f)
  (for-each proc (font-layers f)))

(define (filter-layer proc layer)
  (filter proc (layer-glyphs layer)))


(define (set-layer f new-layer)
  (let ((layers (font-layers f))
        (new-name (layer-name new-layer)))
    (struct-copy font f
                 [layers
                  (dict-values
                   (dict-set (map-layers 
                              (lambda (l) (cons (layer-name l) l)) f)
                             new-name new-layer))])))
   
; get-glyph
; Font, Symbol, Symbol -> Glyph
; Return the given Glyph in the given Layer, Layer defaults to 'public.default

(define (get-glyph f g [l 'public.default])
  (let ([la (get-layer f l)])
    (if la
        (hash-ref (layer-glyphs la) g #f)
        (error "get-glyph: layer does not exist"))))


; get-glyphs
; Font, ListOfSymbol, Symbol -> ListOfGlyph
; Return the given Glyphs in the given Layer, Layer defaults to 'public.default

(define (get-glyphs f gs [l 'public.default])
  (filter identity
          (map (lambda (g) (get-glyph f g l)) gs)))
        

(define (remove-glyph f glyph [layername 'public.default])
  (let ((l (get-layer f layername)))
    (set-layer f (struct-copy layer l 
                              [glyphs (hash-remove (layer-glyphs l) glyph)]))))
    

(define (insert-glyph f glyph [layername 'public.default])
  (let ((l (get-layer f layername)))
    (set-layer f (struct-copy layer l 
                              [glyphs (hash-set (layer-glyphs l)
                                                (glyph-name glyph)                                                              
                                                glyph)]))))
                     
  

(define (get-layers-glyph font glyph)
  (map-layers 
   (lambda (l) 
     (let ([name (layer-name l)])
       (get-glyph font glyph name)))
   font))
  
  
(define (map-glyphs proc o [layer 'public.default] #:sorted [sorted #f])
  (let ([l (cond [(font? o) (get-layer o layer)]
                 [(layer? o ) o]
                 [else (error "map-glyphs: first argument should be a layer or a font")])])
    (if l
        (map proc (if sorted
                      (sort-glyph-list (hash-values (layer-glyphs l)))
                      (hash-values (layer-glyphs l))))
        (error "map-glyphs: layer does not exist"))))

(define (for-each-glyph proc o [layer 'public.default] #:sorted [sorted #f])
  (let ([l (cond [(font? o) (get-layer o layer)]
                 [(layer? o ) o]
                 [else (error "for-each-glyphs: first argument should be a layer or a font")])])
    (if l
        (for-each proc (if sorted
                           (sort-glyph-list (hash-values (layer-glyphs l)))
                           (hash-values (layer-glyphs ))))
        (error "for-each-glyph: layer does not exist"))))


(define (glyphs-in-font f)
  (set->list
    (foldl set-union
           (set)
           (map-layers 
            (lambda (l) 
              (list->set (map-glyphs glyph-name f (layer-name l))))
            f))))

(define (sort-glyph-list gl)
  (sort gl 
        #:key (lambda (g) (symbol->string (glyph-name g)))
        string<?))

; delete
#;
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
               [layers (list 
                        (layer 'public.default #f 
                               (map-glyphs glyph2->glyph1 f)))]))
            
(define (ufo2->ufo3 f) 
  (struct-copy font f [format 3]
               [layers (map-layers
                        (lambda (l)
                          (struct-copy layer l
                                       [glyphs (map-glyphs glyph1->glyph2 f (layer-name l))]))
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
                      [(list) (make-immutable-hash (reverse acc))]
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
                      (write-glyphs (hash-values (layer-glyphs la)) dir)
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

(define (write-ufo font path #:overwrite [overwrite #t] #:proc-data [proc-data #f] #:proc-images [proc-images #f])
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
; Font, Glyph, Symbol -> Glyph
; decompose glyph components to outlines

(define (decompose-glyph f g [ln 'public.default])
  (define (decompose-base c)
    (decompose-glyph f (get-glyph f (component-base c) ln) ln))
  (let* ([cs (glyph-components g)])
    (if (null? cs)
        g
        (let* ([bases (map decompose-base cs)]
               [dcs (apply append (map component->outlines cs bases))])
          (struct-copy glyph g
                       [components null]
                       [contours (append (glyph-contours g) dcs)])))))

#;
(define (decompose-glyph f gn [ln 'public.default])
  (define (decompose-base c)
    (decompose-glyph f (component-base c) ln))
  (let* ([g (get-glyph f gn ln)]
         [cs (glyph-components g)])
    (if (null? cs)
        g
        (let* ([bases (map decompose-base cs)]
               [dcs (apply append (map component->outlines cs bases))])
          (struct-copy glyph g
                       [components null]
                       [contours (append (glyph-contours g) dcs)])))))

; decompose-layer
; Font, Symbol -> Layer
; produces a new layer with glyphs decomposed

(define (decompose-layer f [ln 'public.default])
  (struct-copy layer (get-layer f ln)
               [glyphs (map-glyphs (lambda (g) 
                                     (decompose-glyph f g ln))
                        f ln)]))

                       

;(define (decompose-layer f [ln 'public.default])
;  (let* ([l (get-layer f ln)]
;         [gh (layer-glyphs l)]
;         [dec (lambda (g)
;                (let ([d (apply 
;                              append
;                              (map (lambda (c) 
;                                     (component->outlines
;                                      c (hash-ref gh (component-base c))))
;                                   (glyph-components g)))])
;                  (struct-copy glyph g [components null]
;                               [contours (append (glyph-contours g) d)])))])
;    (struct-copy layer l [glyphs (hash-map gh (lambda (k v) (dec v)))])))


                     
; glyph-bounding-box
; Font, Glyph, Symbol, Boolean -> BoundingBox
; produces the Bounding Box for the given glyph

(define (glyph-bounding-box f g [ln 'public.default] [components #t])
  (let* ([g (if components 
                (decompose-glyph f g ln)
                g)]
         [cs (glyph-contours g)])
    (if (null? cs)
        (cons (vec 0 0) (vec 0 0))
        (apply combine-bounding-boxes 
               (map (lambda (c) 
                      (bezier-bounding-box (contour->bezier c)))
                    cs)))))

; font-bounding-box
; font, Symbol, Boolean -> BoundingBox
; produces the Bounding Box for the given font

(define (font-bounding-box f [ln 'public.default] [components #t])
  (apply combine-bounding-boxes
         (map-glyphs (lambda (g) (glyph-bounding-box f g ln components))
                     f)))


;(define (font-bounding-box f [components #t])
;  (let ([gs (filter
;             (lambda (g) (> (length (glyph-contours g)) 0))
;             (if components (layer-glyphs (decompose-layer f))
;                (layer-glyphs (get-layer f))))])
;    (apply combine-bounding-boxes 
;           (filter identity
;                   (map (lambda (g) 
;                          (apply combine-bounding-boxes
;                                 (map (lambda (c)
;                                        (bezier-bounding-box (contour->bezier c)))
;                                      (glyph-contours g))))
;                        gs)))))

; sidebearings 
; Font, Glyph, Symbol -> (Number . Number)
; produce a pair representing the left and right sidebearings for the given glyph

(define (sidebearings f g [ln 'public.default])
  (let* ([bb (glyph-bounding-box f g ln)]
         [a (advance-width (glyph-advance g))])
    (if (equal? bb (cons (vec 0 0) (vec 0 0)))
        #f
        (cons (vec-x (car bb))
              (- a (vec-x (cdr bb)))))))

; intersections-at 
; font, Glyph, Number, Symbol -> List of Vec
; produces a list of the intersections of outlines with the line y = h

(define (intersections-at f g h [ln 'public.default])
  (let* ([g (decompose-glyph f g ln)]
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
; font, Glyph, Number, Symbol -> (Number . Number)
; produces a pair representing sidebearings measured at y = h

(define (sidebearings-at f g h [ln 'public.default])
  (let* ([is (intersections-at f g h)]
         [a (advance-width (glyph-advance g))])
    (if (null? is)
        #f
        (cons (vec-x (car is)) (- a (vec-x (last is)))))))
    
  
; glyph-signed-area
; font, Glyph, Symbol -> Number
; produces the area for the given glyph (negative if in the wrong direction)

(define (glyph-signed-area f g [ln 'public.default])
  (let* ([g (decompose-glyph f g ln)]
         [cs (glyph-contours g)])
    (foldl + 0 
           (map (lambda (c) 
                  (bezier-signed-area (contour->bezier c)))
                cs))))

; set-sidebearings
; Font, Glyph, Number, Number, Symbol -> Glyph
; set left and right sidebearings for the glyph 

(define (set-sidebearings f g left right [ln 'public.default])
  (let* ([os (sidebearings f g ln)]
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
; Font, Glyph, Number, Number, Number, Symbol -> Glyph
; set left and right sidebearings (measured at y = h) for the glyph 

(define (set-sidebearings-at f g left right h [ln 'public.default])
  (let* ([os (sidebearings-at f g h ln)]
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
; font, Glyph, Number, Number, Symbol -> Glyph
; adjust left and right sidebearings for the glyph

(define (adjust-sidebearings f g left right [ln 'public.default])
  (let* ([os (sidebearings f g ln)])     
    (if os
        (set-sidebearings f 
                          g 
                          (+ (car os) left) 
                          (+ (cdr os) right)
                          ln)
        g)))

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

        
; print-glyph
; Font, Symbol -> side effects
; Print the glyph 
                 
(define (print-glyph f gn)
  (let* ([g (decompose-glyph f (get-glyph f gn))]
         [ascender (hash-ref (font-fontinfo f) 'ascender 750)]
         [upm (hash-ref (font-fontinfo f) 'unitsPerEm 1000)]
         [cs (map-contours contour->bezier g)]
         [bb (if (null? cs)
                 (cons (vec 0 0) (vec 0 0))
                 (apply combine-bounding-boxes
                        (map bezier-bounding-box cs)))])
      (pictf:glyph (draw-glyph g) bb ascender upm)))
                     


