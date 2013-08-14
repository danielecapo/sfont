#lang racket

(require (prefix-in ufo: "ufo.rkt")
         "vec.rkt"
         "fontpict.rkt"
         (prefix-in bz: "bezier.rkt")
         slideshow/pict-convert)

(provide 
 (except-out (all-defined-out)
             sort-by-key
             ->int
             ->float
             ->intlist
             ->widthclass
             filter-common))

;;; KERNING
;;; Kerning data are stored in an association list

;;; INFO
;;; Font Info are stored in an association list

;;; FONT
;;; (font ufo:Font AssociationList AssociationList (listOf Glyph))
(struct font (ufo info kerning glyphs) 
  #:transparent
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (font-info f) 'ascender 750)]
          [descender (dict-ref (font-info f) 'descender -250)]
          [glyphs (map (lambda (g) (draw-glyph (decompose-glyph f g)))
                       (get-glyphs f *text*))])
      (apply pictf:font ascender descender glyphs))))

; Font (listOf Symbol) -> (listOf Glyph)
; produce a list of Glyph in the font whose name is in gs
(define (get-glyphs f gs)
  (let ([g-hash (make-hash (map (lambda (g) 
                                  (cons (glyph-name g) g)) 
                                (font-glyphs f)))])
    (filter identity
            (map (lambda (g) (hash-ref g-hash g #f))
                 gs))))

; Ufo:Font -> Font   
; produce a font from a ufo
(define (ufo->font f)
  (font f
        (interpolable-infos (sort-by-key (hash->list (ufo:font-fontinfo f))))
        (if (ufo:font-kerning f)
            (sort-by-key       (hash-map (ufo:font-kerning f)
                       (lambda (left right)
                         (cons left (sort-by-key (hash->list right))))))
            '())
        (ufo->glyphs f)))

; AssociationList -> AssociationList
; sort the alist with key name
(define (sort-by-key alist)
  (sort alist string<? 
        #:key (lambda (p) 
                (symbol->string (car p)))))

; Font -> Ufo:Font
; produce a ufo font from a font
(define (font->ufo f)
  (struct-copy ufo:font (font-ufo f)
               [fontinfo (ufo-infos f)]
               [kerning (if (null? (font-kerning f))
                            #f
                            (make-immutable-hash
                             (map (lambda (p) (cons (car p) (make-immutable-hash (cdr p))))
                                  (font-kerning f))))]
               [layers (list (struct-copy ufo:layer (ufo:get-layer (font-ufo f))
                                          [glyphs (map (lambda (g) (glyph->ufo-glyph g (font-ufo f)))  
                                                       (font-glyphs f))]))]))


; Utility functions used for infos
(define (->int n) (inexact->exact (round n)))
(define (->float n) (exact->inexact n))
(define (->intlist l) (map ->int l))
(define (->widthclass n) (cond [(< n 0) 0]
                               [(> n 9) 9]
                               [else (->int n)]))


(define *infos*
  `((unitsPerEm ,->int)
    (descender ,->int)
    (xHeight ,->int)
    (capHeight ,->int)
    (ascender ,->int)
    (italicAngle ,->float)
    (openTypeHeadLowestRecPPEM ,->int)
    (openTypeHheaAscender ,->int)
    (openTypeHheaDescender ,->int)
    (openTypeHheaLineGap ,->int)
    (openTypeHheaCaretSlopeRise ,->int)
    (openTypeHheaCaretSlopeRun ,->int)
    (openTypeHheaCaretOffset ,->int)
    (openTypeOS2WidthClass ,->widthclass)
    (openTypeOS2WeightClass ,->int)
    (openTypeOS2Panose ,->intlist)
    (openTypeOS2FamilyClass ,->intlist)
    (openTypeOS2TypoAscender ,->int)
    (openTypeOS2TypoDescender ,->int)
    (openTypeOS2TypoLineGap ,->int)
    (openTypeOS2WinAscent ,->int)
    (openTypeOS2WinDescent ,->int)
    (openTypeOS2SubscriptXSize ,->int)
    (openTypeOS2SubscriptYSize ,->int)
    (openTypeOS2SubscriptXOffset ,->int)
    (openTypeOS2SubscriptYOffset ,->int)
    (openTypeOS2SuperscriptXSize ,->int)
    (openTypeOS2SuperscriptYSize ,->int)
    (openTypeOS2SuperscriptXOffset ,->int)
    (openTypeOS2SuperscriptYOffset ,->int)
    (openTypeOS2StrikeoutSize ,->int)
    (openTypeOS2StrikeoutPosition ,->int)
    (openTypeVheaVertTypoAscender ,->int)
    (openTypeVheaVertTypoDescender ,->int)
    (openTypeVheaVertTypoLineGap ,->int)
    (openTypeVheaCaretSlopeRise ,->int)
    (openTypeVheaCaretSlopeRun ,->int)
    (openTypeVheaCaretOffset ,->int)
    (postscriptSlantAngle ,->int)
    (postscriptUnderlineThickness ,->int)
    (postscriptUnderlinePosition ,->int)
    (postscriptBlueValues ,->intlist)
    (postscriptOtherBlues ,->intlist)
    (postscriptFamilyBlues ,->intlist)
    (postscriptFamilyOtherBlues ,->intlist)
    (postscriptStemSnapH ,->intlist)
    (postscriptStemSnapV ,->intlist)
    (postscriptBlueFuzz ,->int)
    (postscriptBlueShift ,->int)
    (postscriptBlueScale ,->int)
    (postscriptDefaultWidthX ,->int)
    (postscriptNominalWidthX ,->int)))

; Ufo:fontinfo -> Info
; produce an alist from ufo's fontinfo
(define (interpolable-infos infos)
  (let ((keys (map car *infos*)))
    (filter identity
            (dict-map infos (lambda (key value)
                              (if (member key keys)
                                  (cons key value)
                                  #f))))))
; Info -> Ufo:fontinfo 
; produce the ufo's fontinfo from an alist
(define (ufo-infos f)
  (let ((infos (make-immutable-hash 
                (dict-map
                 (font-info f)
                 (lambda (key value)
                   (cons key ((car (dict-ref *infos* key)) value))))))
        (psname (dict-ref (ufo:font-fontinfo (font-ufo f)) 'postscriptFontName "untitled"))
        (famname (dict-ref (ufo:font-fontinfo (font-ufo f)) 'familyName "untitled")))
    (dict-set* infos 'postscriptFontName psname 'familyName famname)))

     
;;; POINT
; point is (list x y)

; Number Number -> Point
(define (pt x y) (list x y))

;;; GLYPH
;;; (glyph Symbol Number (listOf Contours) (listOf Components) (listOf Anchors))
(struct glyph (name advance contours components anchors)
  #:transparent)

; Glyph -> DrawableGlyph
; produce a drawable glyph
(define (draw-glyph g)
   (append (list (glyph-name g)
                 (car (glyph-advance g)))
           (map contour->bezier (glyph-contours g))))


; Contour -> Bezier
; produce a Bezier curve from a Contour
(define (contour->bezier c)
  (map list->vec c))

; Ufo:Glyph -> Glyph
; produce a glyph from a ufo's glyph
(define (ufo-glif->glyph g)
  (glyph (ufo:glyph-name g)
             (let ((a (ufo:glyph-advance g)))
               (pt (ufo:advance-width a) (ufo:advance-height a)))
             (ufo:map-contours ufo->contour g)
             (ufo:map-components ufo->component g)
             (ufo:map-anchors ufo->anchor g)))


; Ufo:Contour -> Contour
; produce a contour from a ufo contour
(define (ufo->contour c)
  (map vec->list (ufo:contour->bezier c)))

; Ufo:Anchor -> Anchor
; produce an anchor froma a ufo anchor
(define (ufo->anchor a)
  (list (ufo:anchor-name a) 
        (vec-x (ufo:anchor-pos a))
        (vec-y (ufo:anchor-pos a))))
 
; Ufo:Component -> Component
; produce a component from a ufo component
(define (ufo->component c)
  (match c
    [(ufo:component base (trans-mat x-scale xy-scale yx-scale y-scale x-offset y-offset) _)
     (list base x-scale xy-scale yx-scale y-scale x-offset y-offset)]))

; Ufo:Font -> (listOf Glyph)
; produce a sorted list of Glyphs from a Ufo
(define (ufo->glyphs f)
  (ufo:map-glyphs ufo-glif->glyph f #:sorted #t))

; Glyph -> Ufo:Glyph
; produce a ufo glyph from a glyph
(define (glyph->ufo-glyph g ufo) 
  (match g
    [(glyph name (list aw ah) contours components anchors)
     (let ((ufo-glyph (ufo:get-glyph ufo name)))
       (struct-copy ufo:glyph ufo-glyph
                    [advance (ufo:advance aw ah)]
                    [anchors (map anchor->ufo anchors)]
                    [components (map component->ufo components)]
                    [contours (map contour->ufo contours)]))]))

; Anchor -> Ufo:Anchor
; produce a ufo anchor from an anchor
(define (anchor->ufo a)
  (match a
   [(list name x y) (ufo:anchor (vec x y) name #f #f)]))

; Component -> Ufo:Component
; produce a component anchor from a component
(define (component->ufo c)
  (match c
   [(list base x-scale xy-scale yx-scale y-scale x-offset y-offset) 
    (ufo:component base 
                   (trans-mat x-scale xy-scale yx-scale 
                              y-scale x-offset y-offset)
                   #f)]))

; Contour -> ufo:contour
; produces a ufo:contour from a FlatContour
(define (contour->ufo fc)
  (ufo:bezier->contour (map list->vec fc)))

; Component, Component -> Component
; produce a new component with scale fields imported from another component
(define (import-component-scale c1 c2)
  (match c1 
    [(list base _ _ _ _ x-offset y-offset)
     (match c2
       [(list _ x-scale xy-scale yx-scale y-scale _ _)
        (list base x-scale xy-scale yx-scale y-scale x-offset y-offset)])]))

; Font (listOf Symbol) -> Font
; produce a font with the glyphs NOT in gs removed
(define (keep-glyphs f gs)
  (struct-copy font f
               [glyphs (filter (lambda (g)
                                 (member (glyph-name g) gs))
                                 (font-glyphs f))]))

; Glyph [(Contour -> Contour)] -> Glyph
; produce a new glyph with contours sorted
(define (sort-contours g [sort-points-fn identity])
  (struct-copy glyph g
               [contours 
                (sort (map sort-points-fn (glyph-contours g))
                      contour<?)]))
      
; Contour Contour -> Boolean
; True if the first contour has fewer points than the second
; or if the first point of the first contours is nearer the origin
; than the first point of the second contour
(define (contour<? c1 c2)
  (let ([l1 (length c1)]
        [l2 (length c2)])
    (or (< l1 l2)
        (and (= l1 l2)
             (< (vec-length (list->vec (car c1)))
                (vec-length (list->vec (car c2))))))))
             
; Contour -> Boolean 
; True if the last point is equal to the first point 
; (and the contour has more than one point)
(define (closed? c)
  (and (> (length c) 1) (equal? (first c) (last c))))

; Contour -> (listOf Points)
; produce a list of the "on curve" points 
; (remove the control points)
(define (on-curve-points c)
  (if (null? (cdr c)) c
      (cons (car c) (on-curve-points (cdddr c)))))

; Point -> Point
; True if x1 < x2, if x coord. are equal true if y1 < y2
(define (point<? pt1 pt2)
  (or (< (car pt1) (car pt2))
      (and (= (car pt1) (car pt2)) 
           (< (cadr pt1) (cadr pt2)))))

; Contour -> Contour
; Rotate the point list by removing the first 'segment' and appeding it to the end of contour
(define (cycle-points c)
  (append (cdddr (take c (- (length c) 1)))
          (take c 4)))

; Contour -> Contour
; Rearrange the contour so that the first point is the mimimum (using point<) of all points
(define (canonical-start-point c)
  (if (closed? c)
      (let ((min-pt (car (sort (on-curve-points c) point<?))))
        (letrec ((aux (lambda (pts)
                        (if (equal? (car pts) min-pt) pts
                            (aux (cycle-points pts))))))
          (aux c)))
      c))

; Glyph -> Glyph
; produce a new glyph trying to fix the directions of contours
(define (correct-directions g)
  (define (area cs)
    (foldl (lambda (c acc) (+ acc (signed-polygonal-area (map list->vec c))))
           0 cs))
  (let ([cs (glyph-contours g)])
    (struct-copy glyph g
                 [contours (if (< (area cs) 0)
                               (map reverse cs)
                               cs)])))
; Glyph -> Glyph
; produce a new glyph with components sorted
(define (sort-components g)
  (struct-copy glyph g
               [components (sort (glyph-components g)
                                 (lambda (c1 c2)
                                   (or (string<? (symbol->string (car c1))
                                                 (symbol->string (car c2)))
                                       (point<? (take-right c1 2) (take-right c2 2)))))]))

; Glyph -> Glyph
; produce a new glyph with anchors sorted
(define (sort-anchors g)
  (struct-copy glyph g
               [anchors (sort (glyph-anchors g)
                              #:key car
                              string<?)]))

; Font [Boolean] -> Font
; Maximize the 'interpolability' of a font (or, at least, that's what we hope to do)
(define (prepare-for-interpolation f [weak #t])
    (struct-copy font f
                 [glyphs (map (lambda (g) (prepare-glyph g weak)) 
                              (font-glyphs f))]))

; Glyph [Boolean] -> Glyph
; Maximize the 'interpolability' of a glyph
; if weak is false, correct directions and reset the first points of contours
(define (prepare-glyph g [weak #t])
  (sort-components 
   (sort-anchors
    (sort-contours (if weak g (correct-directions g)) (if weak identity canonical-start-point)))))

; Font Font -> Font Font
; produce two new interpolable fonts 
(define (compatible-fonts f1 f2)
  (let* ((common (set->list 
                  (set-intersect
                   (list->set (map glyph-name (font-glyphs f1)))
                   (list->set (map glyph-name (font-glyphs f2))))))
         (fc1 (keep-glyphs f1 common))
         (fc2 (keep-glyphs f2 common))
         (compatible-glyphs (filter
                             identity
                             (map compatible-glyphs 
                                 (font-glyphs fc1)
                                 (font-glyphs fc2))))
         (kerning (compatible-kerning
                   (font-kerning f1) 
                   (font-kerning f2)))
         (infos (compatible-infos
                 (font-info f1) 
                 (font-info f2))))
    (values (struct-copy font fc1
                         [glyphs (map car compatible-glyphs)]
                         [kerning (car kerning)]
                         [info (car infos)])
            (struct-copy font fc2
                         [glyphs (map cdr compatible-glyphs)]
                         [kerning (cdr kerning)]
                         [info (cdr infos)]))))

; Kerning Kerning -> (Kerning . Kernig)
; produce a pair of Kerning with the same pairs
(define (compatible-kerning k1 k2)
  (let [(pairs (common-pairs k1 k2))]
    (cons (filter-kerning k1 pairs)
          (filter-kerning k2 pairs))))

; Kerning Kerning -> (listOf (Symbol . Symbol))
; produce a list of kerning pairs used in noth fonts
(define (common-pairs k1 k2)
  (define (kerning-pairs k)
    (foldl (lambda (k acc)
             (append acc
                     (map (lambda (r) 
                            (cons (car k) (car r))) 
                          (cdr k))))
           '()
           k))
  (set->list
   (set-intersect
    (list->set (kerning-pairs k1))
    (list->set (kerning-pairs k2)))))

; Kerning (listOf (Symbol . Symbol)) -> Kerning
; produce a new kerning removing data for combinations NOT in pairs
(define (filter-kerning k1 pairs)
  (define (filter-left k1 left)
    (filter (lambda (k) (member (car k) left)) k1))
  (map (lambda (l)
         (cons (car l)
               (filter (lambda (r)
                         (member (cons (car l) (car r)) pairs))
                       (cdr l))))
       (filter-left k1 (map car pairs))))

; Info Info -> (Info . Info)
; produce a pair of compatible infos  
(define (compatible-infos i1 i2)
  (let* ([common (set->list
                  (set-intersect (list->set (map car i1))
                                 (list->set (map car i2))))]
         [commonf (filter (lambda (i)
                            (if (list? (dict-ref i1 i))
                                (= (length (dict-ref i1 i)) (length (dict-ref i2 i)))
                                #t))
                          common)])
    (cons (sort-by-key (filter-common commonf i1 car))
          (sort-by-key (filter-common commonf i2 car)))))

                
; Glyph Glyph -> (Glyph . Glyph) or False
; if the two Glyph can be made interpolable produce a pair of Glyph, otherwise False
(define (compatible-glyphs g1 g2)
  (let ((c1 (glyph-contours g1))
        (c2 (glyph-contours g2)))
    (if (and (= (length c1) (length c2))
             (andmap (lambda (a b) (= (length a) (length b)))
                     c1 c2))
        (let-values (((c1 c2) (compatible-components (glyph-components g1)
                                                     (glyph-components g2)))
                     ((a1 a2) (compatible-anchors (glyph-anchors g1)
                                                  (glyph-anchors g2))))
          (cons (struct-copy glyph g1 [components c1] [anchors a1])
                (struct-copy glyph g2 [components c2] [anchors a2])))
        #f)))


; (listOf Contour) (listOf Contour) -> (listOf Contour)
; match the second list of contours against the first list.
(define (match-contour-order cs1 cs2)
  (define (contour-distance c1 c2)
    (foldl (lambda (p1 p2 d)
             (+ d (vec-length (vec- (list->vec p2) (list->vec p1)))))
           0 c1 c2))
  (reverse
   (cdr 
    (foldl (lambda (cm acc)
             (let* ([r (cdr acc)]
                    [cs (car acc)]
                    [m (car (argmin cdr (filter (lambda (i) (identity (cdr i)))
                                                (map (lambda (c)
                                                       (cons c
                                                             (if (= (length c) (length cm))
                                                                    (contour-distance cm c)
                                                                    #f)))
                                                     cs))))])
               (cons (remq m cs) (cons m r)))) 
           (cons cs2 '())
           cs1))))


; Glyph Glyph -> Glyph
; match the contours of the second glyph against the first
(define (match-glyphs-contours g1 g2)
  (let ([cs1 (glyph-contours g1)]
        [cs2 (glyph-contours g2)])
    (struct-copy glyph g2 [contours (match-contour-order cs1 cs2)])))

             

; Font, Font -> Font
; match the contours of the second font against the first
(define (match-fonts-contours f1 f2)
  (let ([gs1 (font-glyphs f1)]
        [gs2 (font-glyphs f2)])
    (struct-copy font f2 [glyphs (map match-glyphs-contours gs1 gs2)])))
                  
             
; (listOf Component) (listOf Component) -> (listOf Component) (listOf Component)
; produce two interpolable component lists
(define (compatible-components c1 c2)
  (let ((common-comp (set->list 
                      (set-intersect (list->set (map car c1))
                                     (list->set (map car c2))))))
         (values (filter-common common-comp c1 car)
                 (filter-common common-comp c2 car))))

; (listOf Anchor) (listOf Anchor) -> (listOf Anchor) (listOf Anchor)
; produce two interpolable anchor lists
(define (compatible-anchors a1 a2)
  (let ((common-anc (set->list 
                     (set-intersect (list->set (map car a1))
                                    (list->set (map car a2))))))
         (values (filter-common common-anc a1 car)
                 (filter-common common-anc a2 car))))

; (listOf T1) (listOf T2) (T2 -> T1) ->  (listOf T2) 
; keep the elements i of lst for which (key i) is member of common (?)
(define (filter-common common lst key)
  (filter (lambda (i) (member (key i) common))
          lst))
                     
; Component -> TransformationMatrix                                  
; produce a transformation matrix from a component
(define (component->matrix c)
  (apply trans-mat (cdr c)))

; TransformationMatrix -> Component
; produce a component from a transformation matrix
(define (matrix->component m)
  (match m
    [(trans-mat x-scale xy-scale yx-scale y-scale x-offset y-offset)
     (list x-scale xy-scale yx-scale y-scale x-offset y-offset)]))


; Font Glyph Symbol -> Glyph
; decompose glyph components to outlines
(define (decompose-glyph f g)
  (let* ([cs (glyph-components g)])
    (if (null? cs)
        g
        (let* ([bases (map (lambda (g) (decompose-glyph f g))
                           (get-glyphs f (map car cs)))]
               [dcs (apply append (map component->outlines cs bases))])
          (struct-copy glyph g
                       [components null]
                       [contours (append (glyph-contours g) dcs)])))))

; Component Glyph -> (listOf Contour)
; reduce the component to a list of contour
(define (component->outlines c b)
  (define (transform-contour c m)
    (map (lambda (p)
           (vec->list (transform (list->vec p) m)))
         c))
  (let ([m (component->matrix c)]
        [base-contours (glyph-contours b)])
    (map (lambda (c) (transform-contour c m))
         base-contours)))