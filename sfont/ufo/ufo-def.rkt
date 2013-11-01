#lang racket

(require "../geometry.rkt"
         "../properties.rkt"
         "../fontpict.rkt"
         "../utilities.rkt"
         "../gui/draw-property.rkt"
         racket/generic
         (planet wmfarr/plt-linalg:1:13/matrix)
         slideshow/pict-convert)

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
  [struct font  
  ((fontinfo fontinfo/c) 
   (groups groups/c) 
   (kerning kerning/c)
   (features features/c) 
   (glyphs (or/c (listof glyph?) (hash/c name/c glyph? #:immutable #t)))
   (layers (or/c (listof layer-info?) (hash/c name/c layer-info? #:immutable #t)))
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
  
  [get-layer (->* (glyph?) (name/c) (or/c layer? #f))]
  [map-layers (-> (-> layer? any/c) glyph? (listof any/c))]
  [for-each-layers (-> (-> layer? any/c) glyph? any/c)]
  [filter-glyphs (-> (-> glyph? boolean?) font? (listof glyph?))]
  [set-layer (-> glyph? layer? glyph?)]
  [get-glyph (-> font? name/c (or/c glyph? #f))]
  [get-glyphs (-> font? (listof name/c) (listof glyph?))]
  [remove-glyph (-> font? name/c font?)]
  [insert-glyph (-> font? glyph? font?)]
  [map-glyphs (->* ((-> glyph? any/c) font?) (#:sorted boolean?) (listof any/c))]
  [for-each-glyphs (->* ((-> glyph? any/c) font?) (#:sorted boolean?) any/c)]
  [font-glyphs-list (-> font? (listof glyph?))]
  [sort-glyph-list (->* ((listof glyph?)) (#:key (-> glyph? any/c) #:pred (-> any/c any/c boolean?)) (listof glyph?))]
  [map-kerning (-> (-> real? real?) kerning/c kerning/c)]
  [decompose-glyph (-> font? glyph? glyph?)]
  [decompose-font (-> font? font?)]
  [glyph-bounding-box (case-> (-> glyph? font? bounding-box/c)
                              (-> glyph? bounding-box/c))]
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
  [print-glyph (-> font? name/c any)]
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
  [ensure-number (-> (or/c string? real?) real?)]
  [ensure-symbol (-> (or/c string? symbol? #f) (or/c symbol? #f))]
  [ensure-smooth (-> (or/c string? boolean?) boolean?)]
  [string->color (-> string? color/c)]
  [color->string (-> color/c string?)]
  [ensure-color (-> (or/c string? color/c #f) (or/c color/c #f))]
  [string->unicode (-> string? natural-number/c)]
  [unicode->string (-> natural-number/c string?)]
  [map-contours (-> (-> contour? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-contours (-> (-> contour? any) (or/c glyph? layer?) any)]
  [map-components (-> (-> component? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-components (-> (-> component? any) (or/c glyph? layer?) any)]
  [map-guidelines (-> (-> guideline? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-guidelines (-> (-> guideline? any) (or/c glyph? layer?) any)]
  [map-anchors (-> (-> anchor? any/c) (or/c glyph? layer?) (listof any/c))]
  [for-each-anchors (-> (-> anchor? any) (or/c glyph? layer?) any)]
  [map-points (-> (-> point? any/c) contour? (listof any/c))]
  [for-each-points (-> (-> point? any) contour? any)]
  [layer->layer1 (-> layer? layer?)]
  [anchor->contour (-> anchor? contour?)]
  [contour->bezier (-> contour? bezier/c)]
  [bezier->contour (-> bezier/c contour?)]
  [component->outlines (-> component? glyph? (listof contour?))]
  [contour-open? (-> contour? boolean?)]
  [reverse-contour (-> contour? contour?)]
  [glyph-reverse-directions (-> glyph? glyph?)]
  [glyph-correct-directions (-> glyph? glyph?)]
  [kern-groups2->3 (-> font? font?)]
  [kerning-group-names (-> font? (cons/c (listof name/c) (listof name/c)))]
  [valid-kerning-group-name? (-> name/c (or/c 'left 'right) boolean?)]
  [left-kerning-group? (-> name/c boolean?)]
  [right-kerning-group? (-> name/c boolean?)]
  [kerning-group? (-> name/c boolean?)]
  [update-kerning-group-name (-> name/c (or/c 'left 'right) name/c)]
  [lookup-kerning-pair (-> font? name/c name/c (values real? boolean?))]
  [kerning-value (-> font? name/c name/c real?)])
 draw-glyph
 draw-contour
 draw-points
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
    [(position-based-trans t sup id arg ...)
     #'(define (t o arg ...)
       (struct-copy id o [pos (sup (get-position o) (clean-arg arg) ...)]))]))

(define-syntax (matrix-based-trans stx)
  (syntax-case stx ()
    [(matrix-based-trans t sup id arg ...)
     #'(define (t o arg ...)
       (struct-copy id o [matrix (sup (get-matrix o) (clean-arg arg) ...)]))]))

(define-syntax (compound-based-trans stx)
  (syntax-case stx ()
    [(compound-based-trans field get-field t sup id arg ...)
     #'(define (t o arg ...)
       (struct-copy id o [field (map (lambda (o) (sup o (clean-arg arg) ...))
                                     (get-field o))]))]))

(define-syntax (clean-arg stx)
  (syntax-case stx ()
    [(clean-arg [a d]) #'a]
    [(clean-arg a) #'a]))

(define-syntax-rule (geometric-struct (trans r ...) id expr ...)
  (struct id expr ...
    #:methods gen:geometric
    [(define/generic super-transform transform)
     (define/generic super-translate translate)
     (define/generic super-scale scale)
     (define/generic super-rotate rotate)
     (define/generic super-skew-x skew-x)
     (define/generic super-skew-y skew-y)
     (define/generic super-reflect-x reflect-x)
     (define/generic super-reflect-y reflect-y)
     (trans r ... transform super-transform id m)
     (trans r ... translate super-translate id x y)
     (trans r ... scale super-scale id fx [fy fx])   
     (trans r ... rotate super-rotate id a)
     (trans r ... skew-x super-skew-x id a)
     (trans r ... skew-y super-skew-y id a)
     (trans r ... reflect-x super-reflect-x id)
     (trans r ... reflect-y super-reflect-y id)]))
  

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
                    (if (hash? layers)
                        (if (immutable? layers)
                            layers
                            (make-immutable-hash (hash->list layers)))
                    (make-immutable-hash (map (lambda (l) (cons (layer-info-name l) l)) layers)))
                    lib 
                    data 
                    images))
  #:property prop:draw 
  (lambda (f)
    (let ([ascender (dict-ref (font-fontinfo f) 'ascender 750)]
          [descender (dict-ref (font-fontinfo f) 'descender -250)])
      (lambda (dc leading text size)
          (let ([glyphs (map (lambda (g) (draw-glyph (decompose-glyph f g)))
                             (get-glyphs f (unique-letters text)))])
            (draw-font-dc dc ascender descender leading glyphs (lambda (p) (apply kerning-value f p)) size text)))))
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (font-fontinfo f) 'ascender 750)]
          [descender (dict-ref (font-fontinfo f) 'descender -250)]
          [glyphs (map (lambda (g) (draw-glyph (decompose-glyph f g)))
                       (get-glyphs f (unique-letters (display-text))))])
      (pictf:font ascender descender glyphs (lambda (p) (apply kerning-value f p)))))
  #:methods gen:geometric
  [(define/generic super-transform transform)
   (define/generic super-translate translate)
   (define/generic super-scale scale)
   (define/generic super-rotate rotate)
   (define/generic super-skew-x skew-x)
   (define/generic super-skew-y skew-y)
   (define/generic super-reflect-x reflect-x)
   (define/generic super-reflect-y reflect-y)
   (define (transform f m)
     (apply-font-trans f super-transform m))
   (define (translate f x y)
     (apply-font-trans f super-translate x y))
   (define (scale f fx [fy fx])
     (apply-font-trans f super-scale fx fy))
   (define (rotate f a)
     (apply-font-trans f super-rotate a))
   (define (skew-x f a)
     (apply-font-trans f super-skew-x a))
   (define (skew-y f a)
     (apply-font-trans f super-skew-y a))
   (define (reflect-x f)
     (apply-font-trans f super-reflect-x))
   (define (reflect-y f)
     (apply-font-trans f super-reflect-y))])

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
  #:transparent
  #:methods gen:geometric
  [(define/generic super-transform transform)
   (define/generic super-translate translate)
   (define/generic super-scale scale)
   (define/generic super-rotate rotate)
   (define/generic super-skew-x skew-x)
   (define/generic super-skew-y skew-y)
   (define/generic super-reflect-x reflect-x)
   (define/generic super-reflect-y reflect-y)
   (define (transform l m)
     (apply-layer-trans l super-transform m))
   (define (translate l x y)
     (apply-layer-trans l super-translate x y))
   (define (scale l fx [fy fx])
     (apply-layer-trans l super-scale fx fy))
   (define (rotate l a)
     (apply-layer-trans l super-rotate a))
   (define (skew-x l a)
     (apply-layer-trans l super-skew-x a))
   (define (skew-y l a)
     (apply-layer-trans l super-skew-y a))
   (define (reflect-x l)
     (apply-layer-trans l super-reflect-x))
   (define (reflect-y l)
     (apply-layer-trans l super-reflect-y))])

; Layer  (T . ... -> T) . ... -> Layer
; apply a geometric transformations to a layer
(define (apply-layer-trans l fn . args)
  (let ([t (lambda (o) (apply fn o args))])
    (struct-copy layer l
                 [components (map t (layer-components l))]
                 [anchors (map t (layer-anchors l))]
                 [contours (map t (layer-contours l))])))
 
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
  #:property prop:pict-convertible 
  (lambda (g)
    (let* ([cs (map-contours contour->bezier g)]
           [bb (if (null? cs)
                   (cons (vec 0 0) (vec 0 0))
                   (apply combine-bounding-boxes
                          (map bezier-bounding-box cs)))])
      (pictf:glyph (draw-glyph g) bb 750 1000)))
  #:methods gen:geometric
  [(define/generic super-transform transform)
   (define/generic super-translate translate)
   (define/generic super-scale scale)
   (define/generic super-rotate rotate)
   (define/generic super-skew-x skew-x)
   (define/generic super-skew-y skew-y)
   (define/generic super-reflect-x reflect-x)
   (define/generic super-reflect-y reflect-y)
   (define (transform g m)
     (apply-glyph-trans g super-transform m))
  (define (translate g x y)
    (apply-glyph-trans g super-translate x y))
  (define (scale g fx [fy fx])
    (glyph-scale g fx fy))
  (define (rotate g a)
    (apply-glyph-trans g super-rotate a))
  (define (skew-x g a)
    (apply-glyph-trans g super-skew-x a))
  (define (skew-y g a)
    (apply-glyph-trans g super-skew-y a))
  (define (reflect-x g)
    (apply-glyph-trans g super-reflect-x))
  (define (reflect-y g)
    (apply-glyph-trans g super-reflect-y))])

; Glyph  (T . ... -> T) . ... -> Glyph
; apply a geometric transformations to a glyph
(define (apply-glyph-trans g fn . args)
  (let ([t (lambda (o) (apply fn o args))])
    (struct-copy glyph g [layers (map-layers t g)])))

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
    
; Font Glyph]-> Font
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
     (glyph-bounding-box (decompose-glyph f g) f)]
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

; Font Symbol -> Any
; Print the glyph           
(define (print-glyph f gn)
  (let ([gp (get-glyph f gn)])
    (if (not gp) 
        (error "Glyph not in font")
        (let* ([g (decompose-glyph f (get-glyph f gn))]
               [ascender (hash-ref (font-fontinfo f) 'ascender 750)]
               [upm (hash-ref (font-fontinfo f) 'unitsPerEm 1000)]
               [cs (map-contours contour->bezier g)]
               [bb (if (null? cs)
                       (cons (vec 0 0) (vec 0 0))
                       (apply combine-bounding-boxes
                              (map bezier-bounding-box cs)))])
          (pictf:glyph (draw-glyph g) bb ascender upm)))))
                     
; Font -> Font
; Round the coordinates of the font 
(define (font-round f)
  (struct-copy font f
               [glyphs (map-glyphs layer-round f)]
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

; Glyph -> DrawableGlyph
; produce a printable version of the glyph
(define (draw-glyph g)
  (append (list (glyph-name g)
                (advance-width (glyph-advance g)))
          (map-contours contour->bezier g)))

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
  (match pts
    [(list-rest (point _ 'move _ _ _) pr) pts]
    [(list-rest (point _ 'curve _ _ _) pr) pts]
    [(list-rest (point _ 'line _ _ _) pr) pts]
    [(list-rest (point _ 'qcurve _ _ _) pr) pts]
    [(list-rest (point _ 'offcurve _ _ _) pr) 
     (ensure-first-on-curve (append pr (list (car pts))))]))
           
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
  (letrec ([aux (lambda (pts)
                  (match pts
                    [(list _) '()]
                    [(list-rest (point _ 'offcurve _ _ _) (point v 'line a b c) r)
                     (cons (point v 'qcurve a b c)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'line _ _ _) (point v 'qcurve a b c)  r)
                     (cons (point v 'line a b c)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'line _ _ _) (point _ 'line _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'qcurve _ _ _) (point _ 'offcurve _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'offcurve _ _ _) (point _ 'offcurve _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]
                    [(list-rest (point _ 'offcurve _ _ _) (point _ 'qcurve _ _ _)  r)
                     (cons (cadr pts)
                           (aux (cons (cadr pts) r)))]))])
    (struct-copy contour c
                 [points (let ([pts (reverse (contour-points c))])
                           (aux (append pts (list (car pts)))))])))
    
                  
    
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
; reverse the direction of all contours in the glyph if the area is negative
(define (glyph-correct-directions g)
  (if (< (glyph-signed-area g 30) 0)
      (glyph-reverse-directions g)
      g))


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
    (cond [(> (length k) 1)
           (error (~a "More than one kerning value for the same pair " gl " " gr))]
          [(null? k) (values 0 #f)]
          [else (values (car k) #t)])))

; Font Symbol Symbol -> Number
; produce the kerning value for the pair (without the found flag)
(define (kerning-value f gl gr)
  (let-values ([(k f) (lookup-kerning-pair f gl gr)])
    k))


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



  



