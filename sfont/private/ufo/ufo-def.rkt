#lang racket/base

(require racket/contract/base
         racket/generic
         racket/draw
         racket/dict
         racket/function
         racket/class
         racket/list
         racket/string
         racket/match
         slideshow/pict-convert
         (only-in slideshow/pict
                  pict?
                  dc
                  blank)
         (only-in gls
                  add-method
                  method)
         (for-syntax racket/base
                     racket/syntax)
         "../../geometry.rkt"
         "../../properties.rkt"
         "../pict-parameters.rkt"
         "../pict-utils.rkt"
         "../draw.rkt"
         "../../utilities.rkt"
         )

(provide 
 (contract-out
  [name/c (-> any/c boolean?)]
  [kerning/c (-> any/c boolean?)]
  [groups/c (-> any/c boolean?)]
  [lib/c (-> any/c boolean?)]
  [fontinfo/c (-> any/c boolean?)]
  [features/c (-> any/c boolean?)]
  [images/c (-> any/c boolean?)]
  [data/c (-> any/c boolean?)]
  [color/c (-> any/c boolean?)]
  [foreground name/c]
  [background name/c]
  [sfont-creator string?]
  [glyph-draw-function (parameter/c (case-> (-> glyph? pict?)
                                            (-> glyph? font? pict?)))] 

  [font-draw-function (parameter/c (-> font? pict?))]
  [glyph-in-font-draw-function (parameter/c (-> glyph? font? (is-a?/c dc<%>) void?))]  
  [contours-direction (parameter/c (one-of/c 'ccw 'cw))]
  [struct font  
  ((fontinfo fontinfo/c) 
   (groups groups/c) 
   (kerning kerning/c)
   (features features/c) 
   (glyphs (or/c (listof glyph?) (hash/c name/c glyph? #:immutable #t)))
   (layers (or/c (listof layer-info?) (listof (cons/c name/c layer-info?))))
   (lib lib/c) 
   (data data/c) 
   (images images/c))]
  [struct layer-info 
    ((name name/c) 
     (color (or/c color/c #f))
     (lib lib/c))]
  [struct layer 
    ((name name/c) 
     (guidelines (listof guideline?))
     (anchors (listof anchor?))
     (contours (listof contour?))
     (components (listof component?)))]
  [struct glyph 
    ((name name/c) 
     (advance advance?)
     (unicodes (listof natural-number/c))
     (note (or/c string? #f))
     (image (or/c image? #f))
     (layers (or/c (listof layer?) (hash/c name/c layer? #:immutable #t)))
     (lib lib/c))]
  [struct advance
    ((width real?)
     (height real?))]
  [struct image 
    ((filename string?)
     (matrix trans-mat?) 
     (color (or/c color/c #f)))] 
  [struct guideline 
    ((pos vec?) 
     (angle real?) 
     (name (or/c string? #f))
     (color (or/c color/c #f)) 
     (identifier (or/c symbol? #f)))]
  [struct anchor 
    ((pos vec?)
     (name string?) 
     (color (or/c color/c #f))
     (identifier (or/c symbol? #f)))]
  [struct contour ((identifier (or/c symbol? #f)) (points (listof point?)))]
  [struct component ((base name/c) (matrix trans-mat?) (identifier (or/c symbol? #f)))]
  [struct point 
    ((pos vec?)
     (type (one-of/c 'move 'line 'offcurve 'curve 'qcurve))
     (smooth boolean?)
     (name (or/c string? #f))
     (identifier (or/c symbol? #f)))]
  [set-font-layer (-> font? layer-info? font?)]
  [remove-font-layer (-> font? name/c font?)]
  [get-layer (->* (glyph?) (name/c) (or/c layer? #f))]
  [map-layers (->* ((-> layer? any/c) glyph?) (#:sorted boolean?) (listof any/c))]
  [for-each-layers (->* ((-> layer? any/c) glyph?) (#:sorted boolean?) void?)]
  [filter-glyphs (-> (-> glyph? any/c) font? (listof glyph?))]
  [set-layer (-> glyph? layer? glyph?)]
  [get-glyph (-> font? name/c (or/c glyph? #f))]
  [get-glyphs (-> font? (listof name/c) (listof glyph?))]
  [remove-glyph (-> font? name/c font?)]
  [insert-glyph (-> font? glyph? font?)]
  [map-glyphs (->* ((-> glyph? any/c) font?) (#:sorted boolean?) (listof any/c))]
  [for-each-glyphs (->* ((-> glyph? any/c) font?) (#:sorted boolean?) void?)]
  [font-glyphs-list (-> font? (listof glyph?))]
  [sort-glyph-list (->* ((listof glyph?)) (#:key (-> glyph? any/c) #:pred (-> any/c any/c boolean?)) (listof glyph?))]
  [map-kerning (-> (-> real? real?) kerning/c kerning/c)]
  [decompose-glyph (-> font? glyph? glyph?)]
  [decompose-font (-> font? font?)]
  [glyph-bounding-box (case-> (-> glyph? font? bounding-box/c)
                              (-> glyph? bounding-box/c))]
  [font-bounding-box (->* (font?) (boolean?) bounding-box/c)]
  [get-sidebearings (case-> (-> glyph? (or/c (cons/c real? real?) #f))
                            (-> glyph? font? (or/c (cons/c real? real?) #f)))]
  [intersections-at (case-> (-> glyph? real? (listof vec?))
                            (-> glyph? font? real? (listof vec?)))]
  [get-sidebearings-at (case-> (-> glyph? real? (or/c (cons/c real? real?) #f))
                               (-> glyph? font? real? (or/c (cons/c real? real?) #f)))]
  [glyph-signed-area (case-> (-> glyph? natural-number/c real?)
                             (-> glyph? font? natural-number/c real?))]
  [set-sidebearings (case-> (-> glyph? (or/c real? #f) (or/c real? #f) glyph?)
                            (-> glyph? font? (or/c real? #f) (or/c real? #f) glyph?))]
  [set-sidebearings-at (case-> (-> glyph? (or/c real? #f) (or/c real? #f) real? glyph?)
                               (-> glyph? font? (or/c real? #f) (or/c real? #f) real? glyph?))]
  [adjust-sidebearings (-> glyph? (or/c real? #f) (or/c real? #f) glyph?)]
  [lowercase-stems (-> font? real?)]
  [uppercase-stems (-> font? real?)]
  [correct-directions (-> font? font?)]
  [clockwise-directions (-> font? font?)]
  [counter-clockwise-directions (-> font? font?)]
  [glyph-draw (-> font? name/c pict?)]
  [print-glyph (-> font? name/c void?)]
  [font-round (-> font? font?)]
  [layer-round (-> layer? layer?)]
  [kerning-round (-> kerning/c kerning/c)]
  [glyph-round (-> glyph? glyph?)]
  [advance-round (-> advance? advance?)]
  [image-round (-> image? image?)]
  [guideline-round (-> guideline? guideline?)]
  [anchor-round (-> anchor? anchor?)]
  [contour-round (-> contour? contour?)]
  [component-round (-> component? component?)]
  [point-round (-> point? point?)]
  [string->color (-> string? color/c)]
  [color->string (-> color/c string?)]
  [map-contours (-> (-> contour? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-contours (-> (-> contour? any) (or/c glyph? layer?) void?)]
  [map-components (-> (-> component? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-components (-> (-> component? any) (or/c glyph? layer?) void?)]
  [map-guidelines (-> (-> guideline? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-guidelines (-> (-> guideline? any) (or/c glyph? layer?) void?)]
  [map-anchors (-> (-> anchor? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-anchors (-> (-> anchor? any) (or/c glyph? layer?) void?)]
  [map-points (-> (-> point? any/c) contour? (listof any/c))]
  [for-each-points (-> (-> point? any) contour? void?)]
  [layer->layer1 (-> layer? layer?)]
  [anchor->contour (-> anchor? contour?)]
  [contour->bezier (-> contour? bezier/c)]
  [bezier->contour (-> bezier/c contour?)]
  [component->outlines (-> component? glyph? (listof contour?))]
  [contour-open? (-> contour? boolean?)]
  [contour-clockwise? (-> contour? boolean?)]
  [contour-counter-clockwise? (-> contour? boolean?)]
  [clockwise-contour (-> contour? contour?)]
  [counter-clockwise-contour (-> contour? contour?)]
  [reverse-contour (-> contour? contour?)]
  [glyph-reverse-directions (-> glyph? glyph?)]
  [glyph-correct-directions (-> glyph? glyph?)]
  [glyph-clockwise-directions (-> glyph? glyph?)]
  [glyph-counter-clockwise-directions (-> glyph? glyph?)]
  [kern-groups2->3 (-> font? font?)]
  [kerning-group-names (-> font? (cons/c (listof name/c) (listof name/c)))]
  [valid-kerning-group-name? (-> name/c (or/c 'left 'right) boolean?)]
  [left-kerning-group? (-> name/c boolean?)]
  [right-kerning-group? (-> name/c boolean?)]
  [kerning-group? (-> name/c boolean?)]
  [update-kerning-group-name (-> name/c (or/c 'left 'right) name/c)]
  [lookup-kerning-pair (-> font? name/c name/c (values real? boolean?))]
  [kerning-value (-> font? name/c name/c real?)]
  [sorted-kerning-list (-> kerning/c (listof (cons/c name/c (listof (cons/c name/c real?)))))])
 ;draw-glyph
 ;draw-contour
 ;draw-points
 ==>
 seq)         
             
             
             

;;; Syntax defintion
;;; The following macros are used to create objects that implement the generic interface gen:geometric
;;; there can be three kinds of behaviour:
;;; 1. position-based (points, anchors, ...)
;;; 2. matrix-based   (components, ...)
;;; 3. compound       (contours)

(define-syntax (position-based-trans stx)
  (syntax-case stx ()
    [(position-based-trans pred? id)
     #'(begin 
         (add-method transform
                     (method ((p pred?) (m trans-mat?))
                             (struct-copy id p [pos (transform (get-position p) m)])))
         (add-method translate
                     (method ((p pred?) (x number?) (y number?))
                             (struct-copy id p [pos (translate (get-position p) x y)])))
         (add-method scale
                     (method ((p pred?) (fx number?))
                             (struct-copy id p [pos (scale (get-position p) fx)])))
         (add-method scale
                     (method ((p pred?) (fx number?) (fy number?))
                             (struct-copy id p [pos (scale (get-position p) fx fy)])))
         (add-method rotate
                     (method ((p pred?) (a number?))
                             (struct-copy id p [pos (rotate (get-position p) a)])))
         (add-method skew-x
                     (method ((p pred?) (a number?))
                             (struct-copy id p [pos (skew-x (get-position p) a)])))
         (add-method skew-y
                     (method ((p pred?) (a number?))
                             (struct-copy id p [pos (skew-y (get-position p) a)])))
         (add-method reflect-x
                     (method ((p pred?))
                             (struct-copy id p [pos (reflect-x (get-position p))])))
         (add-method reflect-y
                     (method ((p pred?))
                             (struct-copy id p [pos (reflect-y (get-position p))]))))]))
         
         
(define-syntax (matrix-based-trans stx)
  (syntax-case stx ()
    [(position-based-trans pred? id)
     #'(begin 
         (add-method transform
                     (method ((p pred?) (m trans-mat?))
                             (struct-copy id p [matrix (transform (get-matrix p) m)])))
         (add-method translate
                     (method ((p pred?) (x number?) (y number?))
                             (struct-copy id p [matrix (translate (get-matrix p) x y)])))
         (add-method scale
                     (method ((p pred?) (fx number?))
                             (struct-copy id p [matrix (scale (get-matrix p) fx)])))
         (add-method scale
                     (method ((p pred?) (fx number?) (fy number?))
                             (struct-copy id p [matrix (scale (get-matrix p) fx fy)])))
         (add-method rotate
                     (method ((p pred?) (a number?))
                             (struct-copy id p [matrix (rotate (get-matrix p) a)])))
         (add-method skew-x
                     (method ((p pred?) (a number?))
                             (struct-copy id p [matrix (skew-x (get-matrix p) a)])))
         (add-method skew-y
                     (method ((p pred?) (a number?))
                             (struct-copy id p [matrix (skew-y (get-matrix p) a)])))
         (add-method reflect-x
                     (method ((p pred?))
                             (struct-copy id p [matrix (reflect-x (get-matrix p))])))
         (add-method reflect-y
                     (method ((p pred?))
                             (struct-copy id p [matrix (reflect-y (get-matrix p))]))))]))

(define-syntax (compound-based-trans stx)
  (syntax-case stx ()
    [(compound-based-trans pred? id field get-field)
     #'(begin 
         (add-method transform
                     (method ((p pred?) (m trans-mat?))
                             (struct-copy id p [field (map (lambda (p) (transform p m))
                                                           (get-field p))])))
         (add-method translate
                     (method ((p pred?) (x number?) (y number?))
                             (struct-copy id p [field (map (lambda (p) (translate p x y))
                                                           (get-field p))])))
         (add-method scale
                     (method ((p pred?) (fx number?))
                             (struct-copy id p [field (map (lambda (p) (scale p fx))
                                                           (get-field p))])))
         (add-method scale
                     (method ((p pred?) (fx number?) (fy number?))
                             (struct-copy id p [field (map (lambda (p) (scale p fx fy))
                                                           (get-field p))])))
         (add-method rotate
                     (method ((p pred?) (a number?))
                             (struct-copy id p [field (map (lambda (p) (rotate p a))
                                                           (get-field p))])))
         (add-method skew-x
                     (method ((p pred?) (a number?))
                             (struct-copy id p [field (map (lambda (p) (skew-x p a))
                                                           (get-field p))])))
         (add-method skew-y
                     (method ((p pred?) (a number?))
                             (struct-copy id p [field (map (lambda (p) (skew-y p a))
                                                           (get-field p))])))
         (add-method reflect-y
                     (method ((p pred?))
                             (struct-copy id p [field (map (lambda (p) (reflect-x p))
                                                           (get-field p))])))
         (add-method reflect-y
                     (method ((p pred?))
                             (struct-copy id p [field (map (lambda (p) (reflect-y p))
                                                           (get-field p))]))))]))

(define-syntax (clean-arg stx)
  (syntax-case stx ()
    [(clean-arg [a d]) #'a]
    [(clean-arg a) #'a]))

(define-syntax (geometric-struct stx)
  (syntax-case stx ()
    [(_ (trans r ...) id expr ...)
     (with-syntax ([pred (format-id stx "~a?" #'id)])
       #'(begin
           (struct id expr ...)
           (trans pred id r ...)))]))
    
  

;;; PARAMETERS

; Glyph [Font] -> Pict
; parameter used to draw the glyph in a pict
(define glyph-draw-function 
  (make-parameter (case-lambda [(g) (blank)]
                               [(g f) (blank)])))

; Font -> Pict
; parameter used to draw the font in a pict
(define font-draw-function 
  (make-parameter (lambda (f) (blank))))

; Glyph Font DrawingContext -> Void
; parameter used to draw the glyph in a Drawing Context
; This is used in a "font view" (to print the font in the REPL)
(define glyph-in-font-draw-function 
  (make-parameter (lambda (g f dc) (void))))

; (oneOf ccw cw)
; parameter used to set the 'correct' direction of contours
(define contours-direction
  (make-parameter 'ccw))



;;; CONSTANTS

(define foreground 'public.default)
(define background 'public.background)
(define sfont-creator "com.danielecapo.sfont")

;;;
;;; DATA DEFINITIONS


;;; Names
;;; font glyph and groups names are symbol
(define name/c (flat-named-contract 'name/c symbol?))


;;; Kerning
;;; kernings are represented in an immutable hashtable 
;;; (hashtable (Symbol . (hashtable (Symbol . Real))))
(define kerning/c (flat-named-contract 
                   'kerning/c 
                   (hash/c name/c (hash/c name/c real? #:immutable #t) #:immutable #t #:flat? #t)))

;;; Groups
;;; Groups are represented in an immutable hashtable
;;; (hashtable (Symbol . (listof Symbol)))
(define groups/c (flat-named-contract
                  'groups/c
                  (hash/c name/c (listof name/c) #:immutable #t #:flat? #t)))

;;; Lib
;;; 
(define lib/c (flat-named-contract 
                    'lib/c
                    (hash/c name/c any/c #:immutable #t #:flat? #t)))      

;;; Fontinfo
;;; fontinfo should be defined better
(define fontinfo/c (flat-named-contract 
                    'fontinfo/c
                    (hash/c name/c any/c #:immutable #t #:flat? #t)))      


;;; Features
;;; fontinfo should be defined better
(define features/c (flat-named-contract 'features/c (or/c #f string?)))

;;; Data
;;; fontinfo should be defined better
(define data/c (flat-named-contract 'data/c any/c))

;;; Images
;;; fontinfo should be defined better
(define images/c (flat-named-contract 'images/c any/c))

; Font -> Pict
(define (draw-font f)
  (let* ([ascender (dict-ref (font-fontinfo f) 'ascender 750)]
         [descender (dict-ref (font-fontinfo f) 'descender -250)]
         [glyphs (make-immutable-hash
                  (map (lambda (g) 
                         (cons (glyph-name g)
                               (curry (glyph-in-font-draw-function) g f)))
                       (get-glyphs f (unique-letters (display-text)))))]
         [n-lines (lines (display-text))] 
         [text-height (* (display-size) (+ 1 n-lines (* (- (display-leading) 1) (- n-lines 1))))])
    (dc 
     (lambda (dc dx dy)
       (send dc set-brush (display-brush))
       (send dc set-pen (display-pen))
       (draw-text-in-dc dc ascender descender (display-size) (display-text) (display-leading) glyphs 
                        (if (show-kerning?)
                            (lambda (p) (apply kerning-value f p))
                            (lambda (p) 0))))
     (text-width) text-height)))

; Font -> Pict
(define (font->pict f)
  ((font-draw-function) f))

;;; Font
;;; (font Number String HashTable HashTable HashTable String (listOf Glyph) (listOf Layer) HashTable ... ...)
(struct font 
  (fontinfo groups kerning features glyphs layers lib data images)
  #:guard (lambda (fontinfo groups kerning features glyphs layers lib data images tn)
            (values fontinfo 
                    groups 
                    kerning 
                    features
                    (if (hash? glyphs)
                        (if (immutable? glyphs)
                            glyphs
                            (make-immutable-hash (hash->list glyphs)))
                    (glyphlist->hashglyphs glyphs))
                    (if (dict? layers)
                        layers
                        (map (lambda (l) 
                               (cons (layer-info-name l) l)) 
                             layers))
                    lib 
                    data 
                    images))
  #:property prop:draw 
  (lambda (f)
    (let ([ascender (dict-ref (font-fontinfo f) 'ascender 750)]
          [descender (dict-ref (font-fontinfo f) 'descender -250)])
      (lambda (dc leading text size)
          (let ([glyphs (make-immutable-hash
                  (map (lambda (g) 
                         (cons (glyph-name g)
                               (curry (glyph-in-font-draw-function) g f)))
                       (get-glyphs f (unique-letters text))))])
            (draw-text-in-dc dc ascender descender size text leading glyphs 
                        (if (show-kerning?)
                            (lambda (p) (apply kerning-value f p))
                            (lambda (p) 0)))))))
  #:property prop:pict-convertible font->pict)



; transformations

(define-syntax-rule (add-transformation-font t (arg pred?) ...)
  (add-method t
              (method ((f font?) (arg pred?) ...)
                      (apply-font-trans f t arg ...))))

(add-transformation-font transform (m trans-mat?))

(add-transformation-font translate (x number?) (y number?))

(add-transformation-font scale (fx number?))

(add-transformation-font scale (fx number?) (fy number?))

(add-transformation-font rotate (a number?))

(add-transformation-font skew-x (a number?))

(add-transformation-font skew-y (a number?))

(add-transformation-font reflect-x)

(add-transformation-font reflect-y)

; Font (T -> T) . T1 -> Font
; Produce a new Font applying the transformation
(define (apply-font-trans f fn . args)
  (struct-copy font f
               [glyphs (map-glyphs (lambda (g) (apply fn g args)) f)]))


; (listOf Glyphs) -> (hashTable Symbol Glyph)
; produce an immutable hashtable where keys are the names of glyphs and values are the glyphs
(define (glyphlist->hashglyphs gs)
  (make-immutable-hash 
   (map (lambda (g) (cons (glyph-name g) g))
        gs)))

; (hashTable Symbol Glyphs) -> (listOf Glyphs)
; produce a list of glyphs from hashtables of glyphs
(define (hashglyphs->glyphlist gh)
  (hash-values gh))


;;; LayerInfo
;;; (layer Symbol Color HashTable)
;;; represents information about the layer
(struct layer-info (name color lib) 
  #:transparent)

;;; Layer
;;; (layer Symbol (listOf Guideline) (listOf Anchor) (listOf Contour) (listOf Component))
(struct layer (name guidelines anchors contours components)
  #:transparent)

; Layer  (T . ... -> T) . ... -> Layer
; apply a geometric transformations to a layer
(define (apply-layer-trans l fn . args)
  (let ([t (lambda (o) (apply fn o args))])
    (struct-copy layer l
                 [components (map t (layer-components l))]
                 [anchors (map t (layer-anchors l))]
                 [contours (map t (layer-contours l))])))
 
(define-syntax-rule (add-transformation-layer t (arg pred?) ...)
  (add-method t (method ((l layer?) (arg pred?) ...)
                        (apply-layer-trans l t arg ...))))
             
(add-transformation-layer transform (m trans-mat?))

(add-transformation-layer translate (x number?) (y number?))

(add-transformation-layer scale (fx number?))

(add-transformation-layer scale (fx number?) (fy number?))

(add-transformation-layer rotate (a number?))

(add-transformation-layer skew-x (a number?))

(add-transformation-layer skew-y (a number?))

(add-transformation-layer reflect-x)

(add-transformation-font reflect-y)


; Glyph -> Pict
(define (glyph->pict g)
  ((glyph-draw-function) g))

;;; Glyph
;;; (glyph Natural Symbol Advance (listOf Unicode) String Image (listOf Layer) HashTable)
(struct glyph (name advance unicodes note image layers lib) 
  #:guard (lambda (name advance unicodes note image layers lib tn) 
            (values name advance unicodes note image
             (if (hash? layers)
                 (if (immutable? layers)
                     layers
                     (make-immutable-hash (hash->list layers)))
                 (make-immutable-hash (map (lambda (l) (cons (layer-name l) l)) layers)))
             lib))
  #:transparent
  #:property prop:pict-convertible glyph->pict
  #:property prop:draw (lambda (g) (curry draw-glyph-in-dc g)))


; Glyph  (T . ... -> T) . ... -> Glyph
; apply a geometric transformations to a glyph
(define (apply-glyph-trans g fn . args)
  (let ([t (lambda (o) (apply fn o args))])
    (struct-copy glyph g [layers (map-layers t g)])))

(define-syntax-rule (add-transformation-glyph t (arg pred?) ...)
  (add-method t (method ((l layer?) (arg pred?) ...)
                        (apply-glyph-trans l t arg ...))))
             
(add-transformation-glyph transform (m trans-mat?))

(add-transformation-glyph translate (x number?) (y number?))


(add-method scale 
            (method ((g glyph?) (fx number?))
                    (glyph-scale g fx fx))) 

(add-method scale 
            (method ((g glyph?) (fx number?) (fy number?))
                    (glyph-scale g fx fy))) 

(add-transformation-glyph rotate (a number?))

(add-transformation-glyph skew-x (a number?))

(add-transformation-glyph skew-y (a number?))

(add-transformation-glyph reflect-x)

(add-transformation-glyph reflect-y)

; Glyph Real Real -> Glyph
(define (glyph-scale g fx [fy fx])
  (let ([a (glyph-advance g)])
    (struct-copy glyph (apply-glyph-trans g scale fx fy)
                 [advance (struct-copy advance a
                                       [width  (* (advance-width  a) fx)]
                                       [height (* (advance-height a) fy)])])))

;;; Advance
;;; (advance Number Number)
;;; represent the advance width and height of a glyph
(struct advance (width height) #:transparent)


;;; Image
;;; (image String TransformationMatrix Color)
(geometric-struct (matrix-based-trans)
                  image (filename matrix color) 
                  #:transparent
                  #:property prop:has-matrix (lambda (i) (image-matrix i)))


;;; Guideline
;;; (guideline Vec Number String Color Symbol)
(geometric-struct (position-based-trans)
                  guideline (pos angle name color identifier) 
                  #:transparent
                  #:property prop:has-position (lambda (g) (guideline-pos g)))


;;; Anchor
;;; (anchor Vec String Color Symbol)
(geometric-struct (position-based-trans)
                  anchor (pos name color identifier) 
                  #:transparent
                  #:property prop:has-position (lambda (a) (anchor-pos a)))

;;; Contour
;;; (contour Symbol (listOf Point))

(geometric-struct (compound-based-trans points contour-points) 
                  contour (identifier points) 
                  #:transparent)

;;; Component
;;; (component Symbol TransformationMatrix Symbol)

(geometric-struct (matrix-based-trans)
                  component (base matrix identifier) 
                  #:transparent
                  #:property prop:has-matrix (lambda (c) (component-matrix c)))

;;; Point
;;; (point Vec Symbol Boolean String Symbol)
;;;
;;; Point-type can be one of
;;; - curve
;;; - offcurve
;;; - qcurve
;;; - line
;;; - move


(geometric-struct (position-based-trans)
                  point (pos type smooth name identifier) 
                  #:transparent
                  #:property prop:has-position (lambda (p) (point-pos p)))

;(struct point (pos type smooth name identifier) 
;  #:transparent
;   #:property prop:has-position point-pos
;  #:property prop:transform 
;  (lambda (v m) (point-transform v m)))

;;; Color
;;; (list Number Number Number Number)
;;; the four number represent:
;;; Red     [0, 1]
;;; Green   [0, 1]
;;; Blue    [0, 1]
;;; Alpha   [0, 1]

; Number Number Number Number -> Color
; produce a color
(define (color r g b a)
  (list r g b a))

(define color/c (flat-named-contract 
                 'color/c 
                 (list/c (real-in 0 1) (real-in 0 1) (real-in 0 1) (real-in 0 1))))

;;; Unicode
;;; Unicode is a Number

; Font LayerInfo -> Font
; inserts a new layer in the font layers field
(define (set-font-layer f new-li)
  (let ([new-name (layer-info-name new-li)]
        [layers (font-layers f)])
    (struct-copy font f
                 [layers (if (dict-has-key? layers new-name)
                             (dict-set layers new-name new-li)
                             (append layers
                                     (list (cons new-name new-li))))])))

; Font Symbol -> Font
; remove a layer from the font layers field
(define (remove-font-layer f l)
  (struct-copy font f 
               [layers (dict-remove (font-layers f) l)]))

;; Glyph [Symbol] -> Layer or False
(define (get-layer g [l foreground])
  (hash-ref (glyph-layers g) l #f))

; Glyph Layer -> Glyph
; produce a new font with the layer added (or updated if a layer with the same name already exists)
(define (set-layer g new-layer)
  (let ((layers (glyph-layers g))
        (new-name (layer-name new-layer)))
    (struct-copy glyph g
                 [layers (hash-set layers new-name new-layer)])))

; (Layer -> T) Glyph [Boolean] -> (listOf T)
; apply the procedure to each layer, collect the results in a list 
(define (map-layers proc g #:sorted [sorted #f])
  (map proc (if sorted
                (sort-layer-list (hash-values (glyph-layers g)))
                (hash-values (glyph-layers g)))))

; (Layer -> T) Glyph [Boolean] -> side effects
; apply the procedure to each layer
(define (for-each-layers proc g #:sorted [sorted #f])
  (for-each proc (if sorted
                (sort-layer-list (hash-values (glyph-layers g)))
                (hash-values (glyph-layers g)))))
  
; (listOf Layer) (Layer -> T) (T T -> Boolean) -> (listOf Layer)
; produce a sorted list of layers
(define (sort-layer-list ll 
                         #:key [key (lambda (l) (symbol->string (layer-name l)))]
                         #:pred [pred string<?])
  (sort ll #:key key pred))

; (Glyph -> Boolean) Font-> (listOf Glyphs)
; filter the list of glyphs in the layer with the procedure
(define (filter-glyphs proc f)
  (filter proc (hash-values (font-glyphs f))))


; Font Symbol  -> Glyph or False
; Return the given Glyph in the font
(define (get-glyph f g)
  (hash-ref (font-glyphs f) g #f))

; Font (listOf Symbol) -> (listOf Glyph)
; Return the given Glyphs in the font
(define (get-glyphs f gs)
  (filter identity
          (map (lambda (g) (get-glyph f g)) (remove-duplicates gs))))
        
; Font Symbol -> Font
; produce a new font with the glyph removed from the given layer
(define (remove-glyph f g)
  (struct-copy font f [glyphs (hash-remove (font-glyphs f) g)]))
    
; Font Glyph -> Font
; produce a new font with the glyph inserted in the given layer
(define (insert-glyph f g)
  (struct-copy font f [glyphs (hash-set (font-glyphs f)
                                        (glyph-name g)                                                              
                                        g)]))
                     

; (Glyph -> T) Font Boolean -> (listOf T)
; apply the procedure to each glyph in the font, collects the result in a list
(define (map-glyphs proc f  #:sorted [sorted #f])
  (map proc (if sorted
                (sort-glyph-list (hash-values (font-glyphs f)))
                (hash-values (font-glyphs f)))))

; (Glyph -> T) (Font or Layer) [Symbol] Boolean -> side effects
; apply the procedure to each glyph in the font
(define (for-each-glyphs proc f #:sorted [sorted #f])
  (for-each proc (if sorted
                     (sort-glyph-list (hash-values (font-glyphs f)))
                     (hash-values (font-glyphs f)))))

; Font -> (listof Glyph)
; produce a sorted list of glyphs in the font
(define (font-glyphs-list f)
  (map-glyphs identity f #:sorted #t))


; (listOf Glyph) (Glyph -> T) (T T -> Boolean) -> (listOf Glyph)
; produce a sorted list of glyphs
(define (sort-glyph-list gl 
                         #:key [key (lambda (g) (symbol->string (glyph-name g)))]
                         #:pred [pred string<?])
  (sort gl #:key key pred))

; (Number -> Number) Kerning -> Kerning
; apply the procedure to every kerning value, produce a new kerning table
(define (map-kerning proc k)
  (make-immutable-hash
   (hash-map k (lambda (l kr)
                 (cons l 
                       (make-immutable-hash
                        (hash-map kr (lambda (r v) (cons r (proc v))))))))))

; Font Glyph -> Glyph
; decompose glyph components to outlines
(define (decompose-glyph f g)
  (define (decompose-base c)
    (decompose-glyph f (get-glyph f (component-base c))))
  (let* ([fg (get-layer g foreground)]
         [cs (layer-components fg)])
    (if (null? cs)
        g
        (let* ([bases (map decompose-base cs)]
               [dcs (apply append (map component->outlines cs bases))])
          (set-layer g (struct-copy layer fg
                                    [components null]
                                    [contours (append (layer-contours fg) dcs)]))))))

; Font -> Font
; produces a new font with all layers decomposed
(define (decompose-font f)
  (struct-copy font f [glyphs (map-glyphs 
                               (lambda (g) (decompose-glyph f g))
                               f)]))

; Glyph [Font] -> BoundingBox
; produces the Bounding Box for the given glyph
(define glyph-bounding-box 
  (case-lambda
    [(g f)
     (glyph-bounding-box (decompose-glyph f g))]
    [(g)
     (let ([cs (layer-contours (get-layer g foreground))])
       (if (null? cs)
           #f
           (apply combine-bounding-boxes 
                  (map (lambda (c) 
                         (bezier-bounding-box (contour->bezier c)))
                       cs))))]))

; Font [Boolean] -> BoundingBox
; produces the Bounding Box for the given font
(define (font-bounding-box f [components #t])
  (apply combine-bounding-boxes
         (map-glyphs 
          (lambda (g) (if components 
                          (glyph-bounding-box g f)
                          (glyph-bounding-box g)))
            f)))
 
; Glyph [Font] -> (Real . Real) or False
; produce a pair representing the left and right sidebearings for the given glyph
(define get-sidebearings 
  (case-lambda
    [(g) (let* ([bb (glyph-bounding-box g)]
                [a (advance-width (glyph-advance g))])
           (if (not bb)
               #f
               (cons (vec-x (car bb))
                     (- a (vec-x (cdr bb))))))]
    [(g f) (get-sidebearings (decompose-glyph f g))]))

 
; Glyph [Font] Real -> (listOf Vec) 
; produce a list of the intersections of outlines with the line y = h
(define intersections-at
  (case-lambda
    [(g h) (sort 
          (remove-duplicates
           (apply append 
                  (map-contours 
                   (lambda (c) 
                     (bezier-intersect-hor h (contour->bezier c)))
                   g))
           vec=)
          < #:key vec-x)]
    [(g f h) (intersections-at (decompose-glyph f g) h)]))
  


; Glyph [Font Symbol] Real -> (Real . Real) or False
; produce a pair representing sidebearings measured at y = h
(define get-sidebearings-at
  (case-lambda
    [(g h) (let* ([is (intersections-at g h)]
                  [a (advance-width (glyph-advance g))])
             (if (null? is)
                 #f
                 (cons (vec-x (car is)) (- a (vec-x (last is))))))]
    [(g f h) (get-sidebearings-at (decompose-glyph f g) h)]))
           

; Glyph [Font] Symbol -> Number
; produces the area for the given glyph (negative if in the wrong direction)
(define glyph-signed-area 
  (case-lambda 
    [(g sides) (foldl + 0 
                (map-contours 
                 (lambda (c) 
                   (bezier-signed-area (contour->bezier c) 3 sides))
                 g))]
    [(g f sides) (glyph-signed-area (decompose-glyph f g) f foreground sides)]))
  

; Glyph [Font] (Number or False) (Number or False)  -> Glyph
; set left and right sidebearings for the glyph 
(define set-sidebearings 
  (case-lambda
    [(g left right) 
     (let* ([os (get-sidebearings g)]
            [oa (advance-width (glyph-advance g))])     
       (if os
           (let* ([la (if left (- left (car os)) 0)]
                  [ra (if right (+ la (- right (cdr os))) la)])
             (struct-copy glyph 
                          (translate g la 0)
                          [advance (advance (+ oa ra)
                                            (advance-height 
                                             (glyph-advance g)))]))
           g))]
    [(g f left right) (set-sidebearings (decompose-glyph f g) left right)]))
         

; Glyph [Font] (Number or False) (Number or False) Number  -> Glyph
; set left and right sidebearings (measured at y = h) for the glyph 
(define set-sidebearings-at 
  (case-lambda
    [(g left right h)
     (let* ([os (get-sidebearings-at g h)]
            [oa (advance-width (glyph-advance g))])     
       (if os
           (let* ([la (if left (- left (car os)) 0)]
                  [ra (if right (+ la (- right (cdr os))) la)])
             (struct-copy glyph 
                          (translate g la 0)
                          [advance (advance (+ oa ra)
                                            (advance-height 
                                             (glyph-advance g)))]))
           g))]
    [(g f left right h) 
     (set-sidebearings-at (decompose-glyph f g) left right h)]))
     
        
; Glyph Font (Number or False) (Number or False) -> Glyph
; adjust left and right sidebearings for the glyph
(define (adjust-sidebearings g left right)
  (let* ([a (glyph-advance g)]
         [aw (advance-width a)])
    (if left
        (struct-copy glyph (translate g left 0)
                     [advance (struct-copy advance a
                                           [width (+ aw left (if right right 0))])])
        (if right
            (struct-copy glyph g
                         [advance (struct-copy advance a
                                               [width (+ aw right)])])
            g))))
  
  
; Font -> Number
; produce the value of lowercase stems from n
(define (lowercase-stems f)
  (let ([xh/2 (* 0.5
                 (dict-ref (font-fontinfo f) 
                           'xHeight
                           (/ (dict-ref (font-fontinfo f) 
                                        'ascender)
                              0.67)))])
    (if (hash-has-key? (seq f) 'n)
        (let ([inters (intersections-at (get-glyph f 'n) f xh/2)])
          (if (> (length inters) 2)
              (vec-length (vec- (second inters)
                                (first inters)))
              (error "Sorry I can't determine stems witdth")))
        (error "The font has no \"n\" glyph"))))

; Font -> Number
; produce the value of uppercase stems from H
(define (uppercase-stems f)
  (let ([xh/2 (* 0.5
                 (dict-ref (font-fontinfo f) 
                           'xHeight
                           (/ (dict-ref (font-fontinfo f) 
                                        'ascender)
                              0.67)))])
    (if (hash-has-key? (seq f) 'H)
        (let ([inters (intersections-at (get-glyph f 'H) f xh/2)])
          (if (> (length inters) 2)
              (vec-length (vec- (second inters)
                                (first inters)))
              (error "Sorry I can't determine stems witdth")))
        (error "The font has no \"H\" glyph"))))

; Font -> Font
; produces a new font with contour in the correct direction
(define (correct-directions f)
  (struct-copy font f
               [glyphs (map-glyphs glyph-correct-directions f)])) 

; Font -> Font
; produces a new font with contour in the correct direction
(define (clockwise-directions f)
  (struct-copy font f
               [glyphs (map-glyphs glyph-clockwise-directions f)]))

; Font -> Font
; produces a new font with contour in the correct direction
(define (counter-clockwise-directions f)
  (struct-copy font f
               [glyphs (map-glyphs glyph-counter-clockwise-directions f)]))

; Font Symbol -> Pict
; Print the glyph           
(define (glyph-draw f gn)
  (let ([gp (get-glyph f gn)])
    (if (not gp) 
        (error "Glyph not in font")
        ((glyph-draw-function) (get-glyph f gn) f))))

; Font Symbol -> void
; Print the glyph           
(define (print-glyph f gn)
  (let ([gp (get-glyph f gn)])
    (if (not gp) 
        (print "Glyph not in font")
        (print (glyph-draw f gn)))))
                     
; Font -> Font
; Round the coordinates of the font 
(define (font-round f)
  (struct-copy font f
               [glyphs (map-glyphs glyph-round f)]
               [kerning (kerning-round (font-kerning f))]))

; Layer -> Layer
; Round the coordinates of the layer 
(define (layer-round l)
  (struct-copy layer l
               [guidelines (map guideline-round (layer-guidelines l))]
               [anchors (map anchor-round (layer-anchors l))]
               [contours (map contour-round (layer-contours l))]
               [components (map component-round (layer-components l))]))

; kerning -> kerning
; Round the kerning values 
(define (kerning-round k)
  (map-kerning num->int k))

; Glyph -> Glyph
; Round the coordinates of the glyph 
(define (glyph-round g)
  (struct-copy glyph g
               [advance (advance-round (glyph-advance g))]
               [image (if (glyph-image g)
                          (image-round (glyph-image g))
                          #f)]
               [layers (map-layers layer-round g)]))

; Advance -> Advance
; Round the coordinates of the advance 
(define (advance-round a)
  (struct-copy advance a 
               [width (num->int (advance-width a))]
               [height (num->int (advance-height a))]))

; Image -> Image
; Round the coordinates of the image 
(define (image-round i)
  (parameterize ([precision 1]) 
    (struct-copy image i
                 [matrix (struct-copy trans-mat (image-matrix i))])))
                       
; Guideline -> Guideline
; Round the coordinates of the guideline 
(define (guideline-round g)
  (struct-copy guideline g 
               [pos (vec-round (guideline-pos g))]))

; Anchor -> Anchor
; Round the coordinates of the anchor 
(define (anchor-round a)
  (struct-copy anchor a 
               [pos (struct-copy vec (anchor-pos a))]))

; Contour -> Contour
; Round the coordinates of the contour 
(define (contour-round c)
  (struct-copy contour c 
               [points (map point-round (contour-points c))]))

; Component -> Component
; Round the coordinates of the component 
(define (component-round c)
  (parameterize ([precision 1]) 
   (struct-copy component c 
                [matrix (struct-copy trans-mat (component-matrix c))])))

; Point -> Point
; Round the coordinates of the point
(define (point-round p)
  (struct-copy point p 
               [pos (vec-round (point-pos p))]))


; String -> Color
; produce a color from a string of the type "r,g,b,a"
(define (string->color s)
  (apply color
         (map (lambda (s) (string->number (string-trim s))) 
              (string-split s ","))))

; Color -> String
; produce a string of type "r,g,b,a" from a color
(define (color->string c)
  (string-join (map number->string c) ","))


; (Contour -> T) (Glyph or Layer) -> (listOf T)
; apply the procedure to each contour of the glyph, collect results in a list
(define (map-contours proc o)
  (cond [(layer? o) (map proc (layer-contours o))]
        [(glyph? o) (map proc (layer-contours (get-layer o foreground)))]))

; (Contour -> T) (Glyph or Layer) -> side effects
; apply the procedure to each contour of the glyph
(define (for-each-contours proc o)
  (cond [(layer? o) (for-each proc (layer-contours o))]
        [(glyph? o) (for-each proc (layer-contours (get-layer o foreground)))]))

; (Component -> T) (Glyph or Layer) -> (listOf T)
; apply the procedure to each component of the glyph, collect results in a list
(define (map-components proc o)
  (cond [(layer? o) (map proc (layer-components o))]
        [(glyph? o) (map proc (layer-components (get-layer o foreground)))]))

; (Component -> T) (Glyph or Layer) -> side effects
; apply the procedure to each component of the glyph
(define (for-each-components proc o)
  (cond [(layer? o) (for-each proc (layer-components o))]
        [(glyph? o) (for-each proc (layer-components (get-layer o foreground)))]))

; (Guideline -> T) (Glyph or Layer) -> (listOf T)
; apply the procedure to each guideline of the glyph, collect results in a list
(define (map-guidelines proc o)
  (cond [(layer? o) (map proc (layer-guidelines o))]
        [(glyph? o) (map proc (layer-guidelines (get-layer o foreground)))]))

; (Guideline -> T) (Glyph or Layer) -> side effects
; apply the procedure to each guideline of the glyph
(define (for-each-guidelines proc o)
  (cond [(layer? o) (for-each proc (layer-guidelines o))]
        [(glyph? o) (for-each proc (layer-guidelines (get-layer o foreground)))]))

; (Anchor -> T) (Glyph or Layer) -> (listOf T)
; apply the procedure to each anchor of the glyph, collect results in a list
(define (map-anchors proc o)
  (cond [(layer? o) (map proc (layer-anchors o))]
        [(glyph? o) (map proc (layer-anchors (get-layer o foreground)))]))

; (Anchor -> T) (Glyph or Layer) -> side effects
; apply the procedure to each anchor of the glyph
(define (for-each-anchors proc o)
  (cond [(layer? o) (for-each proc (layer-anchors o))]
        [(glyph? o) (for-each proc (layer-anchors (get-layer o foreground)))]))

; (Point -> T) Contour -> (listOf T)
; apply the procedure to each point of the contour, collect results in a list
(define (map-points proc c)
  (map proc (contour-points c)))

; (Point -> T) Contour -> side effects
; apply the procedure to each point of the contour
(define (for-each-points proc c)
  (for-each proc (contour-points c)))


; Glyph DrawingContext -> void
(define (draw-glyph-in-dc g dc)
  (begin 
    (define path (new dc-path%))
    (for-each-contours (lambda (c) (bezier->path (contour->bezier c) path)) g)
    (send dc draw-path path 0 0 'winding)))

; Glyph DrawingContext -> void
(define (draw-glyph-in-font-view g f dc) 
  (begin
    (draw-glyph-in-dc (decompose-glyph f g) dc)
    (send dc translate (advance-width (glyph-advance g)) 0)))

; Glyph  BoundingBox (Number or False) -> Pict
(define (draw-glyph-view g bb [upm #f])
  (let* ([x-min (bounding-box-min-x bb)]
         [h (bounding-box-height bb)]
         [w (bounding-box-width bb)]
         [y-max (bounding-box-max-y bb)]
         [f (cond [upm (/ (glyph-height) upm)]
                  [(> h 0) (/ (glyph-height) h)]
                  [else 1])])
    (dc 
     (lambda (dc dx dy)
       (begin
         (send dc set-brush (display-brush))
         (send dc set-pen (display-pen))
         (send dc scale f (- f))
         (send dc translate (- (if x-min x-min 0)) (- (if y-max y-max 0)))
         (draw-glyph-in-dc g dc)))
     (* f w) (* f h))))
  


; Glyph [Font] -> Pict
(define draw-glyph
  (case-lambda 
    [(g) 
     (draw-glyph-view g (glyph-bounding-box g) 1000)]
    [(g f)
     (let* ([gd (decompose-glyph f g)]
            [upm (hash-ref (font-fontinfo f) 'unitsPerEm 1000)])
       (draw-glyph-view gd (glyph-bounding-box gd) upm))]))



; Contour -> DrawableContour
; produce a printable version of the contour
(define (draw-contour c)
  (letrec ((aux (lambda (pts)
                  (match pts
                    [(list-rest (point _ 'offcurve _ _ _) rest-points)
                     (aux (append rest-points (list (car pts))))]
                     [_ pts]))))
    (draw-points (aux (contour-points c)))))

; (listOf Point) -> (listOf DrawablePoint)
; produce a printable version of the points
(define (draw-points pts)
  (let* ((first-pt (car pts))
         (rest-pts (cdr pts))
         (start (vec->list (point-pos first-pt))))
    (cons (cons 'move start)
          (append (map (lambda (pt)
                         (match pt 
                           [(point (vec x y) 'offcurve _ _ _)
                            `(off ,x ,y)]
                           [(point (vec x y) _ _ _ _)
                            `(,x ,y)]))
                       rest-pts)
                  (list start)))))


; Layer -> Layer
; produce a new layer compatible with glif1 spec
(define (layer->layer1 l)
  (struct-copy layer l
               [guidelines null]
               [anchors null]
               [contours
                (append 
                 (map-contours 
                  (lambda (c) (match c
                                [(contour _ points)
                                 (contour 
                                  #f (map (lambda (p) 
                                            (struct-copy point p [identifier #f]))
                                          points))]))
                               l)
                 (map-anchors anchor->contour l))]
               [components
                (map-components (lambda (c) (struct-copy component c [identifier #f]))
                                l)]))


; Anchor -> Contour
; produce a contour with one point only that is used by convention in Glif1 to define an anchor
(define (anchor->contour a)
  (contour #f (list (point (anchor-pos a) 'move #f (anchor-name a) #f))))

; (listOf Point) -> (listOf Point) 
; produce a list of points where the first element is always an on-curve point
(define (ensure-first-on-curve pts)
  (if (andmap (lambda (p) (eq? (point-type p) 'offcurve))
              pts)
      pts
      (match pts
        [(list-rest (point _ 'move _ _ _) pr) pts]
        [(list-rest (point _ 'curve _ _ _) pr) pts]
        [(list-rest (point _ 'line _ _ _) pr) pts]
        [(list-rest (point _ 'qcurve _ _ _) pr) pts]
        [(list-rest (point _ 'offcurve _ _ _) pr) 
         (ensure-first-on-curve (append pr (list (car pts))))])))
           
; Contour -> Contour
; produce a contour with the 'smooth' field when needed
(define (auto-smooth c)
  (letrec ([aux 
            (lambda (pts)
              (match pts 
                [(list (point _ _ _ _ _) (point _ _ _ _ _))
                 '()]
                [(list-rest (point _ 'offcurve _ _ _)
                            (point _ 'offcurve _ _ _)
                            _)
                 (cons (cadr pts) (aux (cdr pts)))]
                [(list-rest (point _ _ _ _ _)
                            (point _ 'offcurve _ _ _)
                            _)
                 (cons (cadr pts) (aux (cdr pts)))]
                [(list-rest (point v1 _ _ _ _)
                            (point v2 t s n i)
                            (point v3 _ _ _ _)
                            _)
                 (cons (if (aligned? v1 v2 v3)
                           (point v2 t #t n i)
                           (point v2 t #f n i))
                       (aux (cdr pts)))]))])
    (let* ([pts (contour-points c)]
           [fp (first pts)]
           [sp (second pts)]
           [new-pts (if (contour-open? c)
                        (append (cons fp (aux pts))
                                (list (last pts)))
                        (let [(r (aux (append pts (list fp sp))))]
                          (cons (last r) (drop-right r 1))))])
      (struct-copy contour c [points new-pts]))))
                      
; Contour -> Bezier
; Transform a contour in a cubic bezier curve (i.e. all segments are made by 4 points)
; This function should be rewritten in a less horrible style
(define (contour->bezier c)
  (letrec ((process-quadratic-implicit 
            (lambda (pts [acc '()])
              (match pts
                [(list-rest (point _ 'qcurve _ _ _) pr)
                 (process-quadratic-implicit pr (append acc (list (car pts))))]
                [(list-rest (point v 'offcurve _ _ _)
                            (point v1 'offcurve _ _ _)
                            _)
                 (process-quadratic-implicit 
                  (cdr pts) (append acc (list (car pts)
                                       (point (vec+ v (vec* (vec- v1 v) 0.5))
                                              'qcurve #f #f #f))))]
                [(list (point _ 'offcurve _ _ _)) 
                 (append (cdr acc) (take acc 2))]
                [(list-rest (point _ 'offcurve _ _ _)
                            (point _ 'qcurve _ _ _)
                            pr)
                 (append acc pts)])))            
           (flattener 
            (lambda (pts acc)
              (match pts
                [(list-rest (or
                             (point v 'curve _ _ _)
                             (point v 'move _ _ _)
                             (point v 'line _ _ _))
                            (point v1 'line _ _ _)
                            _)
                 (flattener (cdr pts) (append acc (list v v v1)))]
                ; is this the way lines are written in quadratic contours?
                [(list-rest (point v 'qcurve _ _ _)
                            (point v1 'qcurve _ _ _)
                            _) 
                 (flattener (cdr pts) (append acc (list v v v1)))]
                
                [(list-rest (point v 'qcurve _ _ _)
                            (point v1 'offcurve _ _ _)
                            (point v2 'qcurve _ _ _)
                            _)
                 (flattener (cddr pts) (append acc 
                                       (list v 
                                             (vec+ v (vec* (vec- v1 v) (/ 2 3)))
                                             (vec+ v2 (vec* (vec- v1 v2) (/ 2 3))))))]
                [(list-rest (point _ 'qcurve _ _ _) 
                            (point _ 'offcurve _ _ _) 
                            (point _ 'offcurve _ _ _)
                            _)
                 (flattener (process-quadratic-implicit pts) acc)]
                [(list-rest (point _ 'offcurve _ _ _) 
                            (point _ 'offcurve _ _ _) 
                            (point _ 'offcurve _ _ _)
                            _)
                 (flattener (process-quadratic-implicit pts) acc)]
                [(list (point v 'qcurve _ _ _))
                 (append acc (list v))]
                [(list-rest (point v 'offcurve _ _ _) pr)
                 (flattener pr (append acc (list v)))]
                [(list-rest (point v 'curve _ _ _) pr)
                 (flattener pr (append acc (list v)))]
                [(list-rest (point v 'move _ _ _) pr)
                 (flattener pr (append acc (list v)))]
                [(list-rest (point v 'line _ _ _) pr)
                 (flattener pr (append acc (list v)))]
                [(list) acc]))))
    (let* ((points (ensure-first-on-curve (contour-points c)))
           (first-point (car points)))
      (if (eq? (point-type first-point) 'move)
          (flattener points '())
          (flattener (append points (list first-point)) '())))))


; Bezier -> Contour
; Transform a cubic bezier curve in a contour 
(define (bezier->contour b)
  (letrec ((aux 
            (lambda (prev pts acc)
              (match (cons prev pts)
                [(list-rest (vec x y) (vec x y) (vec x2 y2) (vec x2 y2) rest-pts)
                   (aux (vec x2 y2) rest-pts (append acc (list (point (vec x2 y2) 'line #f #f #f))))]
                [(list-rest (vec x y) (vec ox1 oy1) (vec ox2 oy2) (vec x2 y2) rest-pts)
                 (aux (vec x2 y2) rest-pts (append acc
                                                  (list (point (vec ox1 oy1) 'offcurve #f #f #f)
                                                        (point (vec ox2 oy2) 'offcurve #f #f #f)
                                                        (point (vec x2 y2) 'curve #f #f #f))))]
                [(list _) acc]
                [(list) null]))))
    (let* ((first-pt (car b))
           (ufo-pts (aux first-pt (cdr b) null)))
      (auto-smooth
       (contour #f (if (closed? b) (cons (last ufo-pts) (drop-right ufo-pts 1))
                       (cons (point first-pt 'move #f #f #f)
                             ufo-pts)))))))
  
;
;;;;; WRITE THIS LATER!
;;
;;; Contour -> Contour
;;; transform 'fake' curves in lines, set smooth when needed
;;(define (clean-contour c)
;;  (letrec ([make-lines 
;;            (lambda (pts [acc '()])
;;              (match pts
;;                [(list-rest (point v 'curve _ _ _)
;;                            (point v1 'offcurve s1 n1 i1)
;;                            (point v2 'offcurve s2 n1 i1)
;;                            (point v3 'curve s3 n3 i3)
;;                            _)
;;                 (if (and (aligned? v v1 v2)
;;                          (aligned? v v2 v3))
;;                     (make-lines (cdddr pts)
;;                                 (append acc (list (point v3 'line s3 n3 i3))))
;;                     (make-lines (cdddr pts)
;;                                 (append acc (list (point v1 'offcurve s1 n1 i1)
;;                                                   (point v2 'offcurve s2 n1 i1)
;;                                                   (point v3 'curve s3 n3 i3)))))]
;;                [(list-rest (point v _ _ _ _)
;;                            (point _ 'line _ _ _)
;;                            pr)
;;                 (make-lines (cdr pts) (append acc (cadr pts)))]
;;                [(list p) (append acc (list p))]
;;                [(list) '()]))]
;;           [(set-smooth 
;;             (lambda (pts [acc '()])
;;               (match pts
;;                 [(
;                
;                

; Component Glyph -> (listOf Contour)
; produce a list of contours from a component applying the trasformation matrix to the contours in the base
(define (component->outlines c b)
  (let ([m (component-matrix c)])
    (map-contours (lambda (c) (transform c m))
                  b)))
     
; Contour -> Boolean
; true if the contour direction is clockwise
(define (contour-clockwise? c)
  (clockwise? (map point-pos c)))

; Contour -> Boolean
; true if the contour direction is counterclockwise
(define (contour-counter-clockwise? c)
  (not (contour-clockwise? c)))

; Contour -> Contour
; produce a clockwise contour 
(define (clockwise-contour c)
  (if (contour-clockwise? c) c (reverse-contour c)))

; Contour -> Contour
;  produce a counterclockwise contour 
(define (counter-clockwise-contour c)
  (if (contour-counter-clockwise? c) c (reverse-contour c)))

; Contour -> Boolean
; True if the contour starts with a point of type 'move
(define (contour-open? c)
  (eq? 'move (point-type (car (contour-points c)))))

; Contour -> Boolean?
; True if the contour is quadratic
(define (contour-quadratic? c)
  (letrec ([aux (lambda (lop)
                  (match lop
                    [(list) #t]
                    [(list-rest (point _ 'curve _ _ _) r) #f]
                    [(list-rest (point _ 'line _ _ _) r) #f]
                    [(list-rest (point _ 'qcurve _ _ _) r) #t]
                    [(list-rest (point _ 'offcurve _ _ _) (point _ 'offcurve _ _ _) (point _ 'offcurve _ _ _) r)
                     #t]
                    [(list-rest (point _ 'offcurve _ _ _) r) (aux r)]))])
    (if (= 0 (length (contour-points c))) 
        #f
        (aux (contour-points c)))))
                  
; Contour -> Contour
(define (reverse-cubic c)
  (letrec ([aux (lambda (pts)
                  (match pts
                    [(list _) '()]
                    [(list-rest (point _ 'offcurve _ _ _) (point v 'line a b c) r)
                     (cons (point v 'curve a b c)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'line _ _ _) (point v 'curve a b c)  r)
                     (cons (point v 'line a b c)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'line _ _ _) (point _ 'line _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'curve _ _ _) (point _ 'offcurve _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'offcurve _ _ _) (point _ 'offcurve _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'offcurve _ _ _) (point _ 'curve _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]))])
    (struct-copy contour c
                 [points (let ([pts (reverse (contour-points c))])
                           (aux (append pts (list (car pts)))))])))

; Contour -> Contour
(define (reverse-quadratic c)
  (struct-copy contour c
               [points (reverse (contour-points c))]))
    
                  
    
; Contour -> Contour
; returns the contour with reversed point list
(define (reverse-contour c)
  (cond [(contour-open? c) c]
        [(= (length (contour-points c)) 0) c]
        [(contour-quadratic? c) (reverse-quadratic c)]
        [else (reverse-cubic c)]))


; Glyph -> Glyph
; reverse the direction of all contours in the glyph
(define (glyph-reverse-directions g)
  (struct-copy glyph g 
               [layers (map-layers layer-reverse-directions g)]))

; Layer -> Layer
; reverse the direction of all contours in the layer
(define (layer-reverse-directions l)
  (struct-copy layer l
               [contours (map-contours reverse-contour l)]))

; Glyph -> Glyph
; set the directions of contours using the contours-direction parameter
(define (glyph-correct-directions g)
  (let ([a (glyph-signed-area g 30)])
    (cond [(and (< a 0) (eq? (contours-direction) 'ccw))
           (glyph-reverse-directions g)]
          [(and (< a 0) (eq? (contours-direction) 'cw)) g]
          [(and (> a 0) (eq? (contours-direction) 'ccw)) g]
          [(and (> a 0) (eq? (contours-direction) 'cw))
           (glyph-reverse-directions g)]
          [else g])))

; Glyph -> Glyph
; 
(define (glyph-clockwise-directions g)
  (if (<= (glyph-signed-area g 30) 0)
      g
      (glyph-reverse-directions g)))

; Glyph -> Glyph
; 
(define (glyph-counter-clockwise-directions g)
  (if (>= (glyph-signed-area g 30) 0)
      g
      (glyph-reverse-directions g)))


; Font -> Font
; convert kerning groups' names to UFO3 
(define (kern-groups2->3 f)
  (let* ([gn (kerning-group-names f)]
         [ut (group-name-update-table (car gn) (cdr gn))])
  (struct-copy font f
               [groups (update-group-names (font-groups f) ut)]
               [kerning (update-kern-names (font-kerning f) ut)])))

; Font -> ((listOf Symbol) . (listOf Symbol))
; produce a list of kerning groups' names
(define (kerning-group-names f)
  (let ([g (group-names (font-groups f))]
        [k1 (all-kerning-ref1 (font-kerning f))]
        [k2 (all-kerning-ref2 (font-kerning f))])
    (cons
     (filter (lambda (n) (member n k1)) g)
     (filter (lambda (n) (member n k2)) g))))

; Groups -> (listOf Symbol)
; produce a list of all group names
(define (group-names gs)
  (hash-map gs (lambda (k v) k)))
    
; Kerning -> (listOf Symbol)
; Produce a list of all references used in kerning for the left side
(define (all-kerning-ref1 ks)
  (remove-duplicates
   (hash-map ks (lambda (k v) k))))

; Kerning -> (listOf Symbol)
; Produce a list of all references used in kerning for the right side
(define (all-kerning-ref2 ks)
  (remove-duplicates
   (foldl append  '()
          (hash-map ks (lambda (k v)
                         (hash-map v (lambda (k2 v2) k2)))))))

; (listOf Symbol) (listOf Symbol) -> HashTable (oldname . newname)
; Produce an update table for kerning groups
(define (group-name-update-table gl gr) 
  (define (aux gn side)
    (foldl (lambda (n acc)
           (if (valid-kerning-group-name? n side)
               acc
               (cons (cons n (update-kerning-group-name n side))
                     acc)))
         '()
         gn))
  (make-immutable-hash (append (aux gl 'left) (aux gr 'right))))

; Symbol ('left or 'right) -> Boolean
; True if the group's name complies with UFO3 specs
(define (valid-kerning-group-name? n side)
  (let ([ns (symbol->string n)])
    (cond [(eq? side 'left)
           (starts-with? ns "public.kern1.")]
          [(eq? side 'right)
           (starts-with? ns "public.kern2.")])))

; Symbol -> Boolean
; True if the name is a valid left kerning group name
(define (left-kerning-group? n)
  (starts-with? (symbol->string n) "public.kern1."))

; Symbol -> Boolean
; True if the name is a valid right kerning group name
(define (right-kerning-group? n)
  (starts-with? (symbol->string n) "public.kern2."))

; Symbol -> Boolean
; True if the name is a kerning group 
(define (kerning-group? n)
  (or (left-kerning-group? n)
      (right-kerning-group? n)))

; String String-> Boolean
; True if the string starts with the prefix
(define (starts-with? s pre)
  (if (> (string-length s) (string-length pre))
      (string=? (substring s 0 (string-length pre)) pre)
      #f))
  
; Symbol ('left or 'right) -> Symbol
; produce an updated  kerning group name
(define (update-kerning-group-name n side)
  (let ([ns (symbol->string n)])
    (string->symbol
     (cond [(eq? side 'left)
            (string-append "public.kern1." ns)]
           [(eq? side 'right)
            (string-append "public.kern2." ns)]))))

; Groups HashTable -> Groups
; produce a new group table with updated names
(define (update-group-names gs gut)
  (make-immutable-hash
   (hash-map gs (lambda (k v)
                  (let ([un (hash-ref gut k #f)])
                    (cons (if un un k)
                          (map (lambda (vn) 
                                 (let ([vun (hash-ref gut vn #f)])
                                  (if vun vun vn)))
                               v)))))))

; Kerning HashTable -> Kerning
; produce a new kerning table with updated names
(define (update-kern-names ks gut)
  (make-immutable-hash
   (hash-map ks (lambda (k v)
                  (let ([un (hash-ref gut k #f)])
                    (cons (if un un k)
                          (make-immutable-hash
                           (hash-map v
                                     (lambda (k2 kv) 
                                       (let ([vun (hash-ref gut k2 #f)])
                                         (cons (if vun vun k2) kv)))))))))))

; Font Symbol Symbol -> Number Boolean
; produce the kerning value for the pair, the second value returned is false if the kerning pair is not defined
(define (lookup-kerning-pair f gl gr)
  (define (groups-with-glyph gs g)
    (filter identity
            (hash-map gs (lambda (k v) 
                   (if (member g v) k #f)))))                            
  (let* ([groups (font-groups f)]
         [kerning (font-kerning f)]
         [first-ids (cons gl (groups-with-glyph groups gl))]
         [second-ids (cons gr (groups-with-glyph groups gr))]
         [fg (filter identity
                      (map (lambda (g) (hash-ref kerning g #f))
                           first-ids))]
         [k (filter identity
                    (flatten
                     (map (lambda (g) 
                            (map (lambda (s) (hash-ref g s #f)) second-ids))
                           fg)))])
    (cond ;[(> (length k) 1)
          ; (error (~a "More than one kerning value for the same pair " gl " " gr))]
          [(null? k) (values 0 #f)]
          [else (values (car k) #t)])))

; Font Symbol Symbol -> Number
; produce the kerning value for the pair (without the found flag)
(define (kerning-value f gl gr)
  (let-values ([(k f) (lookup-kerning-pair f gl gr)])
    k))


; Kerning -> AssocationList
(define (sorted-kerning-list k)
  (define (group-first? n1 n2) (kerning-group? n1))
  (sort (hash-map k (lambda (n v)
                      (cons n (sort (hash-map v cons) group-first? #:key car))))
        group-first? #:key car))



(define-syntax ==>
  (syntax-rules (lambda)
    [(==> form) form]
    [(==> form (lambda . a) . r)
     (==> ((lambda . a) form) . r)]
    [(==> form (f . a) . r)
     (==> (f form . a) . r)]
    [(==> form f . r) (==> (f form) . r)]))

(define-syntax seq
  (syntax-rules ()
    [(seq ob a)
     (cond [(symbol? a)
            (hash-ref (seq ob) a)]
           [(number? a)
            (let ([or (seq ob)])
              (if (> a (- (sequence-length or) 1))
                  (error "index out of bounds")
                  (sequence-ref (seq ob) a)))])]            
    [(seq ob)
     (let ([o ob])
       (cond [(font? o) (font-glyphs o)]
             [(glyph? o) (layer-contours (get-layer o foreground))]
             [(layer? o) (layer-contours o)]
             [(contour? o) (contour-points o)]))]
    [(seq ob a . r)
     (==> (seq ob a) (seq . r))]))



  

(glyph-draw-function draw-glyph)
(font-draw-function draw-font)

(glyph-in-font-draw-function draw-glyph-in-font-view)


