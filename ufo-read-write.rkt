#lang racket

(require "plists.rkt"
         "ufo-def.rkt"
         "names.rkt"
         "bezier.rkt"
         "vec.rkt"
         xml
         xml/path)

(provide read-ufo
         write-ufo)

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
  (apply-with-kws make-point (cadr p)))

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
                       (list (make-anchor #:x x #:y y #:name name)))
               elts)]
         [(list-rest 'contour id points)
          (aux (append contours
                       (list (apply-with-kws 
                              make-contour 
                              (append id (list (list 'points (map parse-point points)))))))
               components
               anchors
               elts)]
         [(list 'component args)
          (aux contours
               (append components (list (apply-with-kws
                                         make-component
                                         args)))
               anchors
               elts)])]))
         
  (aux null null null os))

; Xexpr [Symbol or False] -> Glyph
; produce a Glyph from a xexpr representation, if name is not false ovveride the glyph name
(define (xexpr->glyph x [name #f])
  (define (aux acc elts)
    (match elts
      [(list) acc]
      [(list-rest elt restelts)
       (match elt
         
         [(list 'advance args) 
          (aux (struct-copy glyph acc 
                            [advance (apply-with-kws make-advance args)])
               restelts)]
         [(list 'unicode (list (list 'hex hex)))
          (aux (struct-copy glyph acc 
                            [unicodes (append (glyph-unicodes acc) (list (string->unicode hex)))])
               restelts)]
         [(list 'note null n)
          (aux (struct-copy glyph acc [note n])
               restelts)]
         [(list 'image args)
          (aux (struct-copy glyph acc 
                            [image (apply-with-kws make-image args)])
               restelts)]
         [(list 'guideline args)
          (aux (struct-copy glyph acc 
                            [guidelines (cons (apply-with-kws make-guideline args)
                                              (glyph-guidelines acc))])
               restelts)]
         [(list 'anchor args)
          (aux (struct-copy glyph acc 
                            [anchors (cons (apply-with-kws make-anchor args)
                                              (glyph-anchors acc))])
               restelts)]
         [(list-rest 'outline null outlines)
          (let-values ([(contours components anchors) (parse-outlines outlines)])
            (aux (struct-copy glyph
                              (struct-copy glyph acc [contours contours])
                              [components components]
                              [anchors (append (glyph-anchors acc) anchors)])
                 restelts))]
         [(list 'lib null d)
          (aux (struct-copy glyph acc 
                            [lib (xexpr->dict d)])
               restelts)]
         
         [_ acc])]))
  (aux (glyph (string->number (se-path* '(glyph #:format) x))
              (if name name (string->symbol (se-path* '(glyph #:name) x)))
              (make-advance) null #f #f null null null null (make-immutable-hash))
       (se-path*/list '(glyph) x)))

; if the value expr evaluetes to the default value produce the empty list otherwise evaluates expr
(define-syntax-rule (not-default val defaultvalue expr)
    (if (equal? val defaultvalue) '() (list expr)))

; Glyph -> Xexpr
; produce an Xexpr representation of the glyph
(define (glyph->xexpr g)
  (letrec [(aux 
            (lambda (g)
              (match g
                [#f '()]
                [(glyph format name advance codes note image 
                            guidelines anchors contours components lib)
                 `(glyph ((format ,(number->string format))
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
                 `(advance (,@(list `(width ,(number->string width)))
                            ,@(not-default height 0 `(width ,(number->string height)))))]
                [(image filename (trans-mat xs xys yxs ys xo yo) color)
                 `((image ((fileName ,filename)
                           ,@(not-default xs 1 `(xScale ,(number->string xs)))
                           ,@(not-default xys 0 `(xyScale ,(number->string xys)))
                           ,@(not-default yxs 0 `(yxScale ,(number->string yxs)))
                           ,@(not-default ys 1 `(yScale ,(number->string ys)))
                           ,@(not-default xo 0 `(xOffset ,(number->string xo)))
                           ,@(not-default yo 0 `(yOffset ,(number->string yo)))
                           ,@(not-default color #f `(color ,(color->string color))))))]
                
                [(guideline (vec x y) angle name color identifier)
                 `(guideline (,@(not-default x #f `(x ,(number->string x)))
                              ,@(not-default y #f `(y ,(number->string y)))
                              ,@(not-default angle #f `(angle ,(number->string angle)))
                              ,@(not-default name #f `(name ,name))
                              ,@(not-default color #f `(color ,(color->string color)))
                              ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(anchor (vec x y) name color identifier)
                 `(anchor (,@(not-default x #f `(x ,(number->string x)))
                           ,@(not-default y #f `(y ,(number->string y)))
                           ,@(not-default name #f `(name ,name))
                           ,@(not-default color #f `(color ,(color->string color)))
                           ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(contour id points)
                 `(contour (,@(not-default id #f `(identifier ,(symbol->string id))))
                           ,@(map (lambda (p) (aux p)) points))]
                [(point (vec x y) type smooth name id)
                 `(point ((x ,(number->string x)) 
                          (y ,(number->string y))
                          ,@(not-default type 'offcurve `(type ,(symbol->string type)))
                          ,@(not-default smooth #f `(smooth "yes"))
                          ,@(not-default name #f `(name ,name))
                          ,@(not-default id #f `(identifier ,(symbol->string id)))))]
                
                [(component base (trans-mat xs xys yxs ys xo yo) id)
                 `(component ((base ,(symbol->string base))
                              ,@(not-default xs 1 `(xScale ,(number->string xs)))
                              ,@(not-default xys 0 `(xyScale ,(number->string xys)))
                              ,@(not-default yxs 0 `(yxScale ,(number->string yxs)))
                              ,@(not-default ys 1 `(yScale ,(number->string ys)))
                              ,@(not-default xo 0 `(xOffset ,(number->string xo)))
                              ,@(not-default yo 0 `(yOffset ,(number->string yo)))
                              ,@(not-default id #f `(identifier ,(symbol->string id)))))]
      
      )))]
    (aux (if (= (glyph-format g) 1)
             (glyph->glyph1 g)
             (glyph->glyph2 g)))))
             
; String [Symbol or False] -> Glyph
; produce a Glyph read from path, if name is not false ovveride the glyph name
(define (read-glif-file path [name #f])
  (xexpr->glyph
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
                     (xexpr->xml (glyph->xexpr g))
                     '())
                    o)))
    #:exists 'replace))


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
            (cons 'kerning (lambda () (let ([k (read-from-plist (make-ufo-path "kerning.plist"))])
                                        (if k k (make-immutable-hash)))))
            (cons 'features (lambda () (read-from-text-file (make-ufo-path "features.fea"))))
            (cons 'lib (lambda () (let ([l (read-from-plist (make-ufo-path "lib.plist"))])
                                    (if l l (make-immutable-hash)))))
            (cons 'layers read-layers)
            (cons 'data (lambda () (read-from-directory (make-ufo-path "data") proc-data)))
            (cons 'images (lambda () (read-from-directory (make-ufo-path "images") proc-images))))])
    (lambda (k) (dict-ref s k))))


; String (String -> T1) (String -> T2) -> Font
; produce a font read from ufo file in path
(define (read-ufo path #:proc-data [proc-data identity] #:proc-images [proc-images identity])
  (if (directory-exists? path)
      (let* ([reader (reader path proc-data proc-images)]
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
  (font 2 creator
            ((reader 'info))
            ((reader 'groups))
            ((reader 'kerning))
            ((reader 'features))
            ((reader 'layers))
            ((reader 'lib))
            #f
            #f))

; String UfoReader -> Font
; produce a font from an UFO3 file
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

; Font String (String -> ...) (String -> ...) -> UfoWriter
; produce a writer for the ufo file in path
(define (writer font path [proc-data #f] [proc-images #f])
  (define (make-ufo-path file)
    (build-path path file))
  (define (write-on-plist dict path)
    (when (and dict (> (dict-count dict) 0))
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

; Font String [Boolean] (String -> ...) (String -> ...) -> side effects
; write the UFO to the given path
(define (write-ufo f path #:overwrite [overwrite #t] #:proc-data [proc-data #f] #:proc-images [proc-images #f])
  (let ([format (font-format font)]
        [f (if (= format 3) (font->ufo3 f) (font->ufo2 f))] 
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
