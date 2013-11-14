#lang racket

(require "plists.rkt"
         "ufo-def.rkt"
         "names.rkt"
         "../../geometry.rkt"
         xml
         xml/path
         xml/plist)

(provide 
 (contract-out
  [read-ufo (->* (path-string?)  font?)]
  [write-ufo (->* (font? path-string?) 
                 (#:format (list/c 2 3)
                  #:overwrite boolean?)
                 void?)]))


(struct glif (format name advance unicodes 
              note image guidelines anchors 
              contours components lib))


; (listOf (Symbol . T)) -> (listOf keyword)
; produce a list of keywords from a key-value pairs list
(define (collect-keywords kvs)
  (map (lambda (kv) 
         (string->keyword 
          (symbol->string (car kv)))) 
       kvs))

; (listOf (Symbol . T)) -> (listOf T)
; produce a list of values from a key-value pairs list
(define (collect-values kvs)
  (map cadr kvs))

; (... -> T) (listOf (Symbol . T)) -> T
; apply the procedure with the key-value pairs list as arguments
(define (apply-with-kws proc kvs)
  (keyword-apply proc (collect-keywords kvs) (collect-values kvs) '()))
                 
; ... -> Point
; produce a point from ...
(define (parse-point p)
  (apply-with-kws build-point (cadr p)))

; (listOf Outlines) -> (listOf Contour) (listOf Component) (listOf Anchor)
; separate contours, components and anchors
(define (parse-outlines os)
  (define (aux contours components anchors elts)
    (match elts
      [(list) (values contours components anchors)]
      [(list-rest e elts)
       (match e
         [(list 'contour id (list point (list-no-order '(type "move") `(name ,name) `(x ,x) `(y ,y))))
          (aux contours 
               components 
               (append anchors 
                       (list (build-anchor #:x x #:y y #:name name)))
               elts)]
         [(list-rest 'contour id points)
          (aux (append contours
                       (list (apply-with-kws 
                              build-contour 
                              (append id (list (list 'points (map parse-point points)))))))
               components
               anchors
               elts)]
         [(list 'component args)
          (aux contours
               (append components (list (apply-with-kws
                                         build-component
                                         args)))
               anchors
               elts)])]))
         
  (aux null null null os))

; Xexpr [Symbol or False] -> Glif
; produce a Glyph from a xexpr representation, if name is not false ovveride the glyph name
(define (xexpr->glif x [name #f])
  (define (aux acc elts)
    (match elts
      [(list) acc]
      [(list-rest elt restelts)
       (match elt
         
         [(list 'advance args) 
          (aux (struct-copy glif acc 
                            [advance (apply-with-kws build-advance args)])
               restelts)]
         [(list 'unicode (list (list 'hex hex)))
          (aux (struct-copy glif acc 
                            [unicodes (append (glif-unicodes acc) (list (string->unicode hex)))])
               restelts)]
         [(list 'note null n)
          (aux (struct-copy glif acc [note n])
               restelts)]
         [(list 'image args)
          (aux (struct-copy glif acc 
                            [image (apply-with-kws build-image args)])
               restelts)]
         [(list 'guideline args)
          (aux (struct-copy glif acc 
                            [guidelines (cons (apply-with-kws build-guideline args)
                                              (glif-guidelines acc))])
               restelts)]
         [(list 'anchor args)
          (aux (struct-copy glif acc 
                            [anchors (cons (apply-with-kws build-anchor args)
                                              (glif-anchors acc))])
               restelts)]
         [(list-rest 'outline null outlines)
          (let-values ([(contours components anchors) (parse-outlines outlines)])
            (aux (struct-copy glif
                              (struct-copy glif acc [contours contours])
                              [components components]
                              [anchors (append (glif-anchors acc) anchors)])
                 restelts))]
         [(list 'lib null d)
          (aux (struct-copy glif acc 
                            [lib (xexpr->dict d)])
               restelts)]
         
         [_ acc])]))
  (aux (glif (string->number (se-path* '(glyph #:format) x))
              (if name name (string->symbol (se-path* '(glyph #:name) x)))
              (build-advance) null #f #f null null null null (make-immutable-hash))
       (se-path*/list '(glyph) x)))

; if the value expr evaluetes to the default value produce the empty list otherwise evaluates expr
(define-syntax-rule (not-default val defaultvalue expr)
    (if (equal? val defaultvalue) '() (list expr)))

; Number->String
(define (number->ufostring n)
  (number->string
   (if (integer? n)
       (inexact->exact n)
       (exact->inexact n))))

; Glif -> Xexpr
; produce an Xexpr representation of the glyph
(define (glif->xexpr g)
  (letrec [(aux 
            (lambda (g)
              (match g
                [#f '()]
                [(glif format name advance codes note image 
                            guidelines anchors contours components lib)
                 `(glyph ((format ,(number->ufostring format))
                          (name ,(symbol->string name)))
                         ,(aux advance)
                         ,@(map (lambda (c) `(unicode ((hex ,(unicode->string c))))) codes)
                         ,@(not-default note #f `(note () ,note))
                         ;(if note `((note () ,note)) '())
                         ,@(aux image)
                         ,@(map (lambda (guideline) (aux guideline)) guidelines)
                         ,@(map (lambda (anchor) (aux anchor)) anchors)
                         (outline ,@(map (lambda (contour) (aux contour)) contours)
                                  ,@(map (lambda (component) (aux component)) components))
                         ,@(not-default lib (make-immutable-hash) `(lib () ,(dict->xexpr lib)))
                         )]
                [(advance width height)
                 `(advance (,@(list `(width ,(number->ufostring width)))
                            ,@(not-default height 0 `(width ,(number->ufostring height)))))]
                [(image filename (trans-mat xs xys yxs ys xo yo) color)
                 `((image ((fileName ,filename)
                           ,@(not-default xs 1 `(xScale ,(number->ufostring xs)))
                           ,@(not-default xys 0 `(xyScale ,(number->ufostring xys)))
                           ,@(not-default yxs 0 `(yxScale ,(number->ufostring yxs)))
                           ,@(not-default ys 1 `(yScale ,(number->ufostring ys)))
                           ,@(not-default xo 0 `(xOffset ,(number->ufostring xo)))
                           ,@(not-default yo 0 `(yOffset ,(number->ufostring yo)))
                           ,@(not-default color #f `(color ,(color->string color))))))]
                
                [(guideline (vec x y) angle name color identifier)
                 `(guideline (,@(not-default x #f `(x ,(number->ufostring x)))
                              ,@(not-default y #f `(y ,(number->ufostring y)))
                              ,@(not-default angle #f `(angle ,(number->ufostring angle)))
                              ,@(not-default name #f `(name ,name))
                              ,@(not-default color #f `(color ,(color->string color)))
                              ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(anchor (vec x y) name color identifier)
                 `(anchor (,@(not-default x #f `(x ,(number->ufostring x)))
                           ,@(not-default y #f `(y ,(number->ufostring y)))
                           ,@(not-default name #f `(name ,name))
                           ,@(not-default color #f `(color ,(color->string color)))
                           ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(contour id points)
                 `(contour (,@(not-default id #f `(identifier ,(symbol->string id))))
                           ,@(map (lambda (p) (aux p)) points))]
                [(point (vec x y) type smooth name id)
                 `(point ((x ,(number->ufostring x)) 
                          (y ,(number->ufostring y))
                          ,@(not-default type 'offcurve `(type ,(symbol->string type)))
                          ,@(not-default smooth #f `(smooth "yes"))
                          ,@(not-default name #f `(name ,name))
                          ,@(not-default id #f `(identifier ,(symbol->string id)))))]
                
                [(component base (trans-mat xs xys yxs ys xo yo) id)
                 `(component ((base ,(symbol->string base))
                              ,@(not-default xs 1 `(xScale ,(number->ufostring xs)))
                              ,@(not-default xys 0 `(xyScale ,(number->ufostring xys)))
                              ,@(not-default yxs 0 `(yxScale ,(number->ufostring yxs)))
                              ,@(not-default ys 1 `(yScale ,(number->ufostring ys)))
                              ,@(not-default xo 0 `(xOffset ,(number->ufostring xo)))
                              ,@(not-default yo 0 `(yOffset ,(number->ufostring yo)))
                              ,@(not-default id #f `(identifier ,(symbol->string id)))))]
      
      )))]
    (aux g)))
;    (aux (if (= (glyph-format g) 1)
;             (glyph->glyph1 g)
;             (glyph->glyph2 g)))))
             
; String [Symbol or False] -> Glif
; produce a Glyph read from path, if name is not false ovveride the glyph name
(define (read-glif-file path [name #f])
  (xexpr->glif
   (xml->xexpr 
   ((eliminate-whitespace 
     '(glyph advance unicode image guideline anchor 
             outline contour point component lib dict array)
     identity)
   (document-element
    (call-with-input-file path read-xml))))
   name))

; Glyph String -> side effects
; write the glyph to path
(define (write-glif-file g path)
  (call-with-output-file
      path
    (lambda (o)
      (parameterize ([empty-tag-shorthand 'always])
                   (write-xml 
                    (document
                     (prolog (list (p-i (location 1 0 1) 
                                        (location 1 38 39) 
                                        'xml "version=\"1.0\" encoding=\"UTF-8\""))
                             #f '())
                     (xexpr->xml (glif->xexpr g))
                     '())
                    o)))
    #:exists 'replace))

; Glif Symbol -> Layer 
(define (glif->layer g n)
  (match g
    [(glif _ _ _ _ _ _ guidelines anchors contours components _)
     (layer n guidelines anchors contours components)]))

; Glif -> Glyph
(define (glif->glyph g)
  (match g
    [(glif format name advance unicodes 
           note image _ _ _ _ lib)
     (glyph name advance unicodes note image 
            (list (layer foreground null null null null)) lib)]))

; (listof Glyph) (listof (Cons Symbol (HashTable Symbol Glif)))
(define (add-layers-to-glyphs glyphs layer-glifs)
  (map (lambda (g)
         (foldl (lambda (l ga)
                  (let ([glifl (hash-ref (cdr l) (glyph-name g) #f)])
                    (if glifl
                        (struct-copy glyph ga
                                     [layers (hash-set (glyph-layers ga) 
                                                       (car l) 
                                                       (glif->layer glifl (car l)))])
                        ga)))
                g
                layer-glifs))
       glyphs))

; Glyph Symbol -> Glif
(define (glyph->glif g l format)
  (let ([elts (if (get-layer g l)
                  (match (if (= format 1)
                             (layer->layer1 (get-layer g l))
                             (get-layer g l))
                    [(layer _ guidelines anchors contours components)
                     (list guidelines anchors contours components)])
                  (list null null null null))])
  (match g
    [(glyph name advance unicodes 
            note image _ lib)
     (glif format name advance unicodes note image
           (first elts) (second elts)
           (third elts) (fourth elts)
           lib)])))
                        
;

; String (String -> T1) (String -> T2) -> UfoReader
; produce a reader for the ufo file in path
(define (reader path [proc-data identity] [proc-images identity])
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
          (make-immutable-hash))))
  (define (read-layerinfo glyphsdir)
    (read-from-plist 
     (build-path (make-ufo-path glyphsdir) "layerinfo.plist")))
  (define (read-layers)
    (let ([layers (read-from-plist (make-ufo-path "layercontents.plist"))])
      (map (lambda (l)
             (let ([info (read-layerinfo (cadr l))])
               (if info
                   (layer-info
                    (string->symbol (car l))
                    (if (dict-ref info 'color #f)
                        (ensure-color (dict-ref info 'color))
                        #f)
                    (dict-ref info 'lib (make-immutable-hash)))
                   (layer-info (string->symbol (car l)) #f (make-immutable-hash)))))
           (if layers layers (list (list "public.default" "glyphs"))))))
  (define (read-layer-glifs)
    (let ([layers (read-from-plist (make-ufo-path "layercontents.plist"))])
      (map (lambda (l) (cons (string->symbol (car l)) (read-glifs (cadr l))))          
           (if layers layers (list (list "public.default" "glyphs"))))))
  (define (read-glifs glifsdir)
    (let* ([glifspath (make-ufo-path glifsdir)]
           [contents (read-from-plist (build-path glifspath "contents.plist"))])
      (make-hash (map (lambda (g) (cons (glif-name g) g))
                      (hash-map contents
                                (lambda (k v) (read-glif-file (build-path glifspath v) k)))))))
;      (map (lambda (l) 
;             (layer (string->symbol (car l))
;                    (read-layerinfo (cadr l)) 
;                    (read-glyphs (cadr l))))
;           (if layers layers (list (list "public.default" "glyphs"))))))
          
  (define (read-glyphs)
    (let* ([layer-glifs (read-layer-glifs)]
           [glyphs (hash-map (dict-ref layer-glifs foreground)
                           (lambda (k v)
                             (glif->glyph v)))])
      (add-layers-to-glyphs glyphs layer-glifs)))
;    (let* ([glyphspath (make-ufo-path glyphsdir)]
;           [contents (read-from-plist (build-path glyphspath "contents.plist"))])
;      (hash-map contents
;                (lambda (k v) (read-glif-file (build-path glyphspath v) k)))))
           
  (define (read-from-directory path [proc #f])
    (if (directory-exists? path)
        (if proc
            (proc path)
            path)
        #f))
  (let ([s (list 
            (cons 'meta (lambda () (read-from-plist (make-ufo-path "metainfo.plist"))))
            (cons 'info (lambda () (read-from-plist (make-ufo-path "fontinfo.plist"))))
            (cons 'groups read-groups) 
            (cons 'kerning (lambda () (let ([k (read-from-plist (make-ufo-path "kerning.plist"))])
                                        (if k k (make-immutable-hash)))))
            (cons 'features (lambda () (read-from-text-file (make-ufo-path "features.fea"))))
            (cons 'lib (lambda () (let ([l (read-from-plist (make-ufo-path "lib.plist"))])
                                    (if l l (make-immutable-hash)))))
            (cons 'layers read-layers)
            (cons 'glyphs read-glyphs)
            (cons 'data (lambda () (read-from-directory (make-ufo-path "data") proc-data)))
            (cons 'images (lambda () (read-from-directory (make-ufo-path "images") proc-images))))])
    (lambda (k) (dict-ref s k))))


; String (String -> T1) (String -> T2) -> Font
; produce a font read from ufo file in path
(define (read-ufo path )
  (if (directory-exists? path)
      (let* ([reader (reader path identity identity)]
             [meta ((reader 'meta))]
             [format (dict-ref meta 'formatVersion)]
             [creator (dict-ref meta 'creator)])
        (cond [(= format 2) (kern-groups2->3  (read-ufo2 creator reader))]
              [(= format 3) (read-ufo3 creator reader)]
              [#t (error "I can only read ufo 2 and ufo 3 fonts")]))
      (error "file do not exists")))

; String UfoReader -> Font
; produce a font from an UFO2 file
(define (read-ufo2 creator reader)
  (font ((reader 'info))
        ((reader 'groups))
        ((reader 'kerning))
        ((reader 'features))
        ((reader 'glyphs))
        ((reader 'layers))
        ((reader 'lib))
        #f
        #f))

; String UfoReader -> Font
; produce a font from an UFO3 file
(define (read-ufo3 creator reader)
  (font ((reader 'info))
        ((reader 'groups))
        ((reader 'kerning))
        ((reader 'features))
        ((reader 'glyphs))
        ((reader 'layers))
        ((reader 'lib))
        ((reader 'data))
        ((reader 'images))))

(define (layer-info->hash l)
  (let ([clr (if (layer-info-color l)
                 (list (cons 'color (color->string (layer-info-color l))))
                 null)]
        [lib (if (layer-info-lib l)
                 (list (cons 'lib (layer-info-lib l)))
                 null)])
    (make-immutable-hash (append clr lib))))

; Font String (String -> ...) (String -> ...) -> UfoWriter
; produce a writer for the ufo file in path
(define (writer f path format [proc-data #f] [proc-images #f])
  (define (make-ufo-path file)
    (build-path path file))
  (define (write-on-plist dict path)
    (when (and dict (> (dict-count dict) 0))
      (write-dict dict path)))
  (define (write-kerning k path)
    (let* ([k-list (sorted-kerning-list k)]
           [k-plist 
            (cons 'dict
                  (for/list ([el k-list])
                    `(assoc-pair ,(symbol->string (car el))
                                 ,(cons 'dict
                                        (for/list ([el2 (cdr el)])
                                          `(assoc-pair ,(symbol->string (car el2))
                                                       ,(dict->plist (cdr el2))))))))])
      (call-with-output-file path
        (lambda (out) 
          (write-plist k-plist out))
        #:exists 'replace)))
                                              
  (define (write-directory dir path [proc #f])
    (when dir
      (if proc
          (proc path)
          (copy-directory/files dir path))))
  (define (write-on-text-file text path)
    (when text
      (let ([text (string-trim text)])
        (when (and (string? text) (not (string=? "" text)))
          (call-with-output-file path 
            (lambda (o)
              (write-string text o)))))))
  (define (write-groups)
    (write-on-plist (make-immutable-hash
                     (hash-map (font-groups f)
                               (lambda (name content)
                                 (cons name (map symbol->string content)))))
                    (make-ufo-path "groups.plist")))
  (define (get-layers-names)
    (letrec ([aux (lambda (acc layers names)
                    (match layers
                      [(list) acc]
                      [(list-rest (layer-info 'public.default _ _) rest-layers)
                       (aux (cons (cons 'public.default "glyphs") acc)
                            rest-layers
                            (cons "glyphs" names))]
                      [(list-rest (layer-info l _ _) rest-layers)
                       (let ([name (namesymbol->filename l "glyphs." "" names)])
                                (aux (cons (cons l name) acc)
                                     rest-layers
                                     (cons name names)))]))])                
      (reverse (aux '() (hash-values (font-layers f)) '()))))
  
  (define layers-names (get-layers-names))
  (define (write-glyphs l glyphsdir)
    (letrec ([aux (lambda (glyphs acc names)
                    (match glyphs
                      [(list) (make-immutable-hash (reverse acc))]
                      [(list-rest g rest-glyphs)
                       (let* ([name (namesymbol->filename (glyph-name g) "" ".glif" names)]
                              [gl (if (get-layer g l)
                                      (glyph->glif g l (if (= format 3) 2 1))
                                      #f)])                        
                          (begin
                            (when gl (write-glif-file gl (build-path glyphsdir name)))
                            (aux rest-glyphs 
                                 (if gl
                                     (cons (cons (glyph-name g) name) acc)
                                     acc)
                                 (cons name names))))]))])
      (write-on-plist (aux (hash-values (font-glyphs f)) '() '())
                      (build-path glyphsdir "contents.plist"))))
      
  (define (write-layers)
    (let* ([l (if (= format 3)
                  (font-layers f)
                  (hash foreground (hash-ref (font-layers f) foreground)))])
      (for-each (lambda (l)
                  (begin
                    (let ([dir (make-ufo-path (cdr l))]
                          [la (car l)])
                      (make-directory dir)
                      (write-glyphs la dir)
                      (when (= format 3)
                        (write-layerinfo la dir)))))
                layers-names)))
  (define (write-layerinfo l dir)
    (let ([li (hash-ref (font-layers f) l)])
      (when (and li
                 (or (layer-info-color li)
                     (> (hash-count (layer-info-lib li)) 0)))
        (write-on-plist (layer-info->hash li) (build-path dir "layerinfo.plist")))))
  (define (write-layercontents)
    (write-on-plist 
     (map (lambda (layer) (list (symbol->string (car layer)) (cdr layer)))
          layers-names)
     (make-ufo-path "layercontents.plist")))
  (let ([s (list 
            (cons 'meta (lambda () 
                          (write-on-plist (hash 'creator sfont-creator
                                                'formatVersion format)
                                          (make-ufo-path "metainfo.plist"))))
            (cons 'info (lambda () 
                          (write-on-plist (font-fontinfo f) 
                                          (make-ufo-path "fontinfo.plist"))))
            (cons 'groups write-groups)
            (cons 'kerning (lambda () (write-kerning (font-kerning f) 
                                                      (make-ufo-path "kerning.plist"))))
            (cons 'features (lambda () (write-on-text-file (font-features f)
                                                           (make-ufo-path "features.fea"))))
            (cons 'lib (lambda () (write-on-plist (font-lib f) 
                                                  (make-ufo-path "lib.plist"))))
            (cons 'layers write-layers)
            (cons 'layercontents write-layercontents)
            (cons 'data (lambda () (write-directory (font-data f) (make-ufo-path "data") proc-data)))
            (cons 'images (lambda () (write-directory (font-images f) (make-ufo-path "images") proc-images))))])
    (lambda (k) (dict-ref s k))))

; Font String [Boolean] (String -> ...) (String -> ...) -> side effects
; write the UFO to the given path
(define (write-ufo f path #:format [format 2] #:overwrite [overwrite #t])
  (unless (and (filename-extension path) 
               (string=? (bytes->string/utf-8 (filename-extension path))
                         "ufo"))
    (error (format "Expected ufo extension, but given ~a" path)))
  (let ([writer (writer f path format #f #f)])
    (if (and (directory-exists? path) (not overwrite))
        (error "The file already exists, use #:overwrite to force writing")
        (begin
          (if (directory-exists? path)
              (clean-ufo-dir path)
              (make-directory path))
          (cond [(= format 2) (write-ufo2 writer)]
                [(= format 3) (write-ufo3 writer)]
                [#t (error "I can only write Ufo 2 and Ufo 3 files")])))))

; PathString -> Void
; Delete ufo files from ufo dir
(define (clean-ufo-dir path)
  (begin
    (unless (and (filename-extension path) 
                 (string=? (bytes->string/utf-8 (filename-extension path))
                           "ufo"))
      (error (format "Expected ufo extension, but given ~a" path)))
    (unless (directory-exists? path)
      (error (format "The path ~a does not exist" path)))
    (let ([files (map (curry build-path path)
                      (list "metainfo.plist"
                            "fontinfo.plist"
                            "groups.plist"
                            "kerning.plist"
                            "features.fea"
                            "lib.plist"
                            "layercontents.plist"))]
          [dirs (map (curry build-path path)
                     (list "glyphs" "images" "data"))]
          [gdirs (map (curry build-path path)
                      (filter (lambda (s)
                                (and (> (string-length s) 7)
                                     (string=? "glyphs."
                                               (substring s 0 7))))
                              (map path->string (directory-list path))))])
      (begin
        (for-each (lambda (f) (when (file-exists? f)
                                (delete-file f)))
                  files)
        (for-each (lambda (d) (when (directory-exists? d)
                                (delete-directory/files d)))
                  dirs)
        (for-each (lambda (d) (when (directory-exists? d)
                                (delete-directory/files d)))
                  gdirs)))))

; ufoWriter -> side effects
; write an UFO2 with the UfoWriter
(define (write-ufo2 writer)
  (begin
    ((writer 'meta))
    ((writer 'info))
    ((writer 'groups))
    ((writer 'kerning))
    ((writer 'features))
    ((writer 'layers))
    ((writer 'lib))))

; ufoWriter -> side effects
; write an UFO3 with the UfoWriter
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



; functions for reading data

; (String or Number) -> Number
; produce a number from a string or return the number
(define (ensure-number n)
  (if (or (not n) (number? n)) 
      n 
      (string->number (string-replace n "," "."))))

; (String or Symbol) -> Symbol
; produce a symbol from a string or return the symbol
(define (ensure-symbol s)
  (if s
      (if (symbol? s) s (string->symbol s))
      s))

; ("yes" or "no" or Boolean) -> Boolean
; produce a boolean from yes/no strings or return the boolean
(define (ensure-smooth s)
  (match s
    [#f #f]
    ["no" #f]
    [#t #t]
    ["yes" #t]
    [_ (error "invalid value for smooth")]))

; (String or Color) -> Color
; produce a color from the string or return the color
(define (ensure-color c)
  (if (string? c) (string->color c) c))

; String -> Unicode
; produce an Unicode from String
(define (string->unicode s)
  (string->number (string-append "#x" s)))

; Unicode -> String 
; produce a String from an Unicode
(define (unicode->string n)
  (~r n #:base '(up 16) #:pad-string "0" #:min-width 4))


(define (build-advance #:width [width 0] #:height [height 0])
  (advance (ensure-number width) (ensure-number height)))

(define (build-image #:fileName filename #:xScale [x-scale 1] #:xyScale [xy-scale 0] 
                        #:yxScale [yx-scale 0] #:yScale [y-scale 1] #:xOffset [x-offset 0]
                        #:yOffset [y-offset 0] #:color [color #f])
  (image filename (trans-mat (ensure-number x-scale) (ensure-number xy-scale) 
                             (ensure-number yx-scale) (ensure-number y-scale) 
                             (ensure-number x-offset) (ensure-number y-offset))
         (ensure-color color)))


(define (build-guideline #:x x #:y y  #:angle angle 
                            #:name [name #f] #:color [color #f] 
                            #:identifier [identifier #f])
  (guideline (vec (ensure-number x) (ensure-number y)) (ensure-number angle) name 
             (ensure-color color) (ensure-symbol identifier)))

(define (build-anchor #:x x #:y y #:name name
                     #:color [color #f] #:identifier [identifier #f])
  (anchor (vec (ensure-number x) (ensure-number y)) name (ensure-color color) (ensure-symbol identifier)))

(define (build-contour #:identifier [identifier #f] #:points [points null])
  (contour (ensure-symbol identifier) points))

(define (build-component #:base base #:xScale [x-scale 1] #:xyScale [xy-scale 0] 
                        #:yxScale [yx-scale 0] #:yScale [y-scale 1] #:xOffset [x-offset 0]
                        #:yOffset [y-offset 0] #:identifier [identifier #f])
  (component (ensure-symbol base) 
             (trans-mat (ensure-number x-scale) (ensure-number xy-scale) 
                        (ensure-number yx-scale) (ensure-number y-scale) 
                        (ensure-number x-offset) (ensure-number y-offset))
             (ensure-symbol identifier)))

(define (build-point #:x x #:y y #:type [type 'offcurve] 
                        #:smooth [smooth #f] #:name [name #f] #:identifier [identifier #f])
  (point (vec (ensure-number x) (ensure-number y)) (ensure-symbol type)
             (ensure-smooth smooth) name (ensure-symbol identifier)))