#lang racket

;; This file borrows concepts from fontMath by Tal Leming (https://github.com/typesupply/fontMath)
;; in particular I have used its list of 'interpolable' Fontinfos 

(require "../ufo.rkt"
         "../fontpict.rkt"
         "../geometry.rkt"
         "../gui/draw-property.rkt"
         slideshow/pict-convert
         racket/generic)

(provide 
 (contract-out
  [fkerning/c (-> any/c boolean?)]
  [finfo/c (-> any/c boolean?)]
  [fanchor/c (-> any/c boolean?)]
  [fcomponent/c (-> any/c boolean?)]
  [struct ffont ((ufo font?)
                 (info finfo/c)
                 (kerning fkerning/c)
                 (glyphs (listof fglyph?)))]
  [struct fglyph ((name name/c)
                  (advance (list/c real? real?))
                  (contours (listof fcontour?))
                  (components (listof fcomponent/c))
                  (anchors (listof fanchor/c)))]
  [struct fcontour ((identifier (or/c symbol? #f)) (points (listof vec?)))]
  [ffont-scale (->* (ffont? real?) (real?) ffont?)]
  [ffont-scale-glyphs (->* (ffont? real?) (real?) ffont?)]
  [info-scale (->* (finfo/c real?) (real?) finfo/c)]
  [kerning-scale (-> fkerning/c real? fkerning/c)]
  [fget-glyphs (-> ffont? (listof name/c) (listof fglyph?))]
  [ufo->ffont (-> font? ffont?)]
  [ffont->ufo (-> ffont? font?)]
  [fglyph-scale (->* (fglyph? real?) (real?) fglyph?)]
  [prepare-for-interpolation (->* (ffont?) (boolean?) ffont?)]
  [prepare-glyph (->* (fglyph?) (boolean?) fglyph?)]
  [compatible-fonts (-> ffont? ffont? (values ffont? ffont?))]
  [compatible-glyphs (-> fglyph? fglyph? (or/c (cons/c fglyph? fglyph?) #f))]
  [compatible-kerning (-> fkerning/c fkerning/c (cons/c fkerning/c fkerning/c))]
  [compatible-infos (-> finfo/c finfo/c (cons/c finfo/c finfo/c))]
  [compatible-components (-> (listof fcomponent/c) (listof fcomponent/c) 
                             (values (listof fcomponent/c) (listof fcomponent/c)))]
  [compatible-anchors (-> (listof fanchor/c) (listof fanchor/c)
                          (values (listof fanchor/c) (listof fanchor/c)))]
  [match-glyphs-contours (-> fglyph? fglyph? fglyph?)]
  [match-fonts-contours (-> ffont? ffont? ffont?)]
  [import-component-scale (-> fcomponent/c fcomponent/c fcomponent/c)]
  [glyph->fglyph (-> glyph? fglyph?)]
  [fglyph->glyph (->* (fglyph?) (font?) glyph?)]
  ))
 
 #;
 (except-out (all-defined-out)
             sort-by-key
             ->int
             ->float
             ->intlist
             ->widthclass
             filter-common)
 
 
 ; Now, I want to remove the duplicaton of 'fonts'
 
 
 ; Make two interpolable font
 ; make every glyph interpolable remove the non interpolable ones
 ; remove reference to removed glyphs from kerning and groups
 ; remove kerning-pair not presents in both fonts
 ; remove info not present in both fonts
 
 
 ; make every glyph interpolable remove the non interpolable ones
 ; if the glyph is not present in both fonts remove it
 ; if the number of contours is different in the two fonts remove the glyph
 ; remove line from contours
 ; order contours
 ; if the contours don't match remove the glyph
 ; - remove all the reference to the removed glyph in components
 ; sort components
 ; remove components if not in both glyph
 ; sort anchors
 ; remove anchors if not in both glyph

 ; remove kerning-pair not presents in both fonts
 ; for every left kerning name
 ; - if not present in both fonts remove it
 ; - otherwise
 ;   - collect every right name in the first font
 ;   - collect every right name in the second font
 ;   - remove every kerning entry whose right name is not in both fonts
 
 ; remove info not present in both fonts
 
 
 
 
;;; FKERNING
;;; Kerning data are stored in an association list
;;;'((left1
;;;   (right1 . value)
;;;   (right2 . value))
;;;  (left2
;;;   (... . ...)
;;;   (... . ...)))
 
(define fkerning/c 
  (flat-named-contract 'fkerning/c 
                       (listof (cons/c name/c (listof (cons/c name/c real?))))))

;;; FINFO
;;; Font Info are stored in an association list

(define finfo/c 
  (flat-named-contract 'finfo/c
                       (listof (cons/c symbol? any/c))))

(define fcomponent/c (flat-named-contract 'fcomponent/c component?))
(define fanchor/c (flat-named-contract 'fanchor/c anchor?))



;;; FFONT
;;; (font Font FInfo FKerning (listOf FGlyph))
(struct ffont (ufo info kerning glyphs) 
  #:transparent
  #:property prop:draw 
  (lambda (f)
    (let ([ascender (dict-ref (ffont-info f) 'ascender 750)]
          [descender (dict-ref (ffont-info f) 'descender -250)])
      (lambda (dc leading text size)
        (let ([glyphs (map (lambda (g) (draw-fglyph (decompose-fglyph f g)))
                           (fget-glyphs f (unique-letters text)))])
          (draw-font-dc dc ascender descender leading glyphs (lambda (p) 0) size text)))))
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (ffont-info f) 'ascender 750)]
          [descender (dict-ref (ffont-info f) 'descender -250)]
          [glyphs (map (lambda (g) (draw-fglyph (decompose-fglyph f g)))
                       (fget-glyphs f (unique-letters (TEXT))))])
      (pictf:font ascender descender glyphs)))
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
     (apply-ffont-trans f super-transform m))
   (define (translate f x y)
     (apply-ffont-trans f super-translate x y))
   (define (scale f fx [fy fx])
     (ffont-scale f fx fy))
   (define (rotate f a)
     (apply-ffont-trans f super-rotate a))
   (define (skew-x f a)
     (apply-ffont-trans f super-skew-x a))
   (define (skew-y f a)
     (apply-ffont-trans f super-skew-y a))
   (define (reflect-x f)
     (apply-ffont-trans f super-reflect-x))
   (define (reflect-y f)
     (apply-ffont-trans f super-reflect-y))])

; FFont (T -> T) . T1 -> FFont
; Produce a new Font applying the transformation
(define (apply-ffont-trans f fn . args)
  (struct-copy ffont f
               [glyphs (map (lambda (g) (apply fn g args))
                            (ffont-glyphs f))]))

; FFont  Number [Number] -> FFont
; Produce a new Font scaling contours infos and kerning
(define (ffont-scale f fx [fy fx])
  (struct-copy ffont f
               [glyphs (map (lambda (g) (scale g fx fy))
                            (ffont-glyphs f))]
               [info (info-scale (ffont-info f) fx fy)]
               [kerning (kerning-scale (ffont-kerning f) fx)]))

; Font Number [Number] -> Font
; produce a new font with glyphs and kerning scaled (do not affect info)
(define (ffont-scale-glyphs f fx [fy fx])
  (struct-copy ffont f
               [kerning (kerning-scale (ffont-kerning f) fx)]
               [glyphs (map (lambda (g) (scale g fx fy))
                            (ffont-glyphs f))]))

; Info Number [Number] -> Info
; Scale Info
(define (info-scale i fx [fy fx])
  (dict-map i (lambda (key value)
                (cons key 
                      (((car (dict-ref *info-transform* key)) 
                        * value) fx fy)))))

; Kerning Number -> Kerning
; Scale kerning
(define (kerning-scale kern f)
  (map (lambda (k)
         (cons (car k) 
               (map (lambda (r)
                      (cons (car r) (* (cdr r) f)))
                    (cdr k))))
       kern))

;;; Functions used for info

(define (->x fn n)
  (lambda (x y)
    (fn n x)))

(define (->amb fn n)
  (lambda (x y)
    (fn n (/ (+ x y) 2))))

(define (->y fn n)
  (lambda (x y)
    (fn n y)))

(define (->list-y fn lst)
  (lambda (x y)
    (map (lambda (n) (fn n y)) lst)))

(define (->list-x fn lst)
  (lambda (x y)
    (map (lambda (n) (fn n x)) lst)))

(define (->list-amb fn lst)
  (lambda (x y)
    (map (lambda (n) (fn n (/ (+ x y) 2))) lst)))


(define *info-transform*
  `((unitsPerEm ,->y)
    (descender ,->y)
    (xHeight ,->y)
    (capHeight ,->y)
    (ascender ,->y)
    (italicAngle ,->amb)
    (openTypeHeadLowestRecPPEM ,->amb)
    (openTypeHheaAscender ,->y)
    (openTypeHheaDescender ,->y)
    (openTypeHheaLineGap ,->y)
    (openTypeHheaCaretSlopeRise ,->y)
    (openTypeHheaCaretSlopeRun ,->y)
    (openTypeHheaCaretOffset ,->y)
    (openTypeOS2WidthClass ,->x)
    (openTypeOS2WeightClass ,->amb)
    (openTypeOS2Panose ,->list-amb)
    (openTypeOS2FamilyClass ,->list-amb)
    (openTypeOS2TypoAscender ,->y)
    (openTypeOS2TypoDescender ,->y)
    (openTypeOS2TypoLineGap ,->y)
    (openTypeOS2WinAscent ,->y)
    (openTypeOS2WinDescent ,->y)
    (openTypeOS2SubscriptXSize ,->x)
    (openTypeOS2SubscriptYSize ,->y)
    (openTypeOS2SubscriptXOffset ,->x)
    (openTypeOS2SubscriptYOffset ,->y)
    (openTypeOS2SuperscriptXSize ,->x)
    (openTypeOS2SuperscriptYSize ,->y)
    (openTypeOS2SuperscriptXOffset ,->x)
    (openTypeOS2SuperscriptYOffset ,->y)
    (openTypeOS2StrikeoutSize ,->y)
    (openTypeOS2StrikeoutPosition ,->y)
    (openTypeVheaVertTypoAscender ,->y)
    (openTypeVheaVertTypoDescender ,->y)
    (openTypeVheaVertTypoLineGap ,->y)
    (openTypeVheaCaretSlopeRise ,->y)
    (openTypeVheaCaretSlopeRun ,->y)
    (openTypeVheaCaretOffset ,->y)
    (postscriptSlantAngle ,->amb)
    (postscriptUnderlineThickness ,->amb)
    (postscriptUnderlinePosition ,->y)
    (postscriptBlueValues ,->list-y)
    (postscriptOtherBlues ,->list-y)
    (postscriptFamilyBlues ,->list-y)
    (postscriptFamilyOtherBlues ,->list-y)
    (postscriptStemSnapH ,->list-y)
    (postscriptStemSnapV ,->list-x)
    (postscriptBlueFuzz ,->y)
    (postscriptBlueShift ,->y)
    (postscriptBlueScale ,->y)
    (postscriptDefaultWidthX ,->x)
    (postscriptNominalWidthX ,->x)))

; Font (listOf Symbol) -> (listOf Glyph)
; produce a list of Glyph in the font whose name is in gs
(define (fget-glyphs f gs)
  (let ([g-hash (make-hash (map (lambda (g) 
                                  (cons (fglyph-name g) g)) 
                                (ffont-glyphs f)))])
    (filter identity
            (map (lambda (g) (hash-ref g-hash g #f))
                 gs))))

; Ufo:Font -> Font   
; produce a font from a ufo
(define (ufo->ffont f)
  (ffont f
         (interpolable-infos (sort-by-key (hash->list (font-fontinfo f))))
         (if (font-kerning f)
             (sort-by-key (hash-map (font-kerning f)
                                    (lambda (left right)
                                      (cons left (sort-by-key (hash->list right))))))
             '())
         (ufo->fglyphs f)))

; AssociationList -> AssociationList
; sort the alist with key name
(define (sort-by-key alist)
  (sort alist string<? 
        #:key (lambda (p) 
                (symbol->string (car p)))))

; FFont -> Font
; produce a ufo font from a font
(define (ffont->ufo f)
  (struct-copy font (ffont-ufo f)
               [fontinfo (ufo-infos f)]
               [kerning (if (null? (ffont-kerning f))
                            (make-immutable-hash)
                            (make-immutable-hash
                             (map (lambda (p) (cons (car p) (make-immutable-hash (cdr p))))
                                  (ffont-kerning f))))]
               [layers (list (struct-copy layer (get-layer (ffont-ufo f))
                                          [glyphs (map (lambda (g) (fglyph->glyph g (ffont-ufo f)))  
                                                       (ffont-glyphs f))]))]))


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

; Info -> FInfo
; produce an alist from ufo's fontinfo
(define (interpolable-infos infos)
  (let ((keys (map car *infos*)))
    (filter identity
            (dict-map infos (lambda (key value)
                              (if (member key keys)
                                  (cons key value)
                                  #f))))))
; FFont -> Info 
; produce the ufo's fontinfo from an alist
(define (ufo-infos f)
  (let ((infos (make-immutable-hash 
                (dict-map
                 (ffont-info f)
                 (lambda (key value)
                   (cons key ((car (dict-ref *infos* key)) value))))))
        (psname (dict-ref (font-fontinfo (ffont-ufo f)) 'postscriptFontName "untitled"))
        (famname (dict-ref (font-fontinfo (ffont-ufo f)) 'familyName "untitled")))
    (dict-set* infos 'postscriptFontName psname 'familyName famname)))

     
;;; FPOINT
; FPoint is (vec x y)


; Number Number -> Point
(define (pt x y) (vec x y))


;;; FGLYPH
;;; (glyph Symbol (list Real Real) (listOf FContours) (listOf FComponents) (listOf FAnchors))
(struct fglyph (name advance contours components anchors)
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
   (define (transform g m)
     (apply-fglyph-trans g super-transform m))
   (define (translate g x y)
     (apply-fglyph-trans g super-translate x y))
   (define (scale g fx [fy fx])
     (fglyph-scale g fx fy))
   (define (rotate g a)
     (apply-fglyph-trans g super-rotate a))
   (define (skew-x g a)
     (apply-fglyph-trans g super-skew-x a))
   (define (skew-y g a)
    (apply-fglyph-trans g super-skew-y a))
   (define (reflect-x g)
     (apply-fglyph-trans g super-reflect-x))
   (define (reflect-y g)
     (apply-fglyph-trans g super-reflect-y))])

; FContour (FPoint Args ... -> FPoint) Args ... -> FContour
(define (apply-fcontour-trans c proc . args)
  (struct-copy fcontour c [points (map (lambda (p) (apply proc p args))
                                       (fcontour-points c))]))

(struct fcontour (identifier points)
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
   (define (transform c m)
     (apply-fcontour-trans c super-transform m))
   (define (translate c x y)
     (apply-fcontour-trans c super-translate x y))
   (define (scale c fx [fy fx])
     (apply-fcontour-trans c super-scale fx fy))
   (define (rotate c a)
     (apply-fcontour-trans c super-rotate a))
   (define (skew-x c a)
     (apply-fcontour-trans c super-skew-x a))
   (define (skew-y c a)
    (apply-fcontour-trans c super-skew-y a))
   (define (reflect-x c)
     (apply-fcontour-trans c super-reflect-x))
   (define (reflect-y c)
     (apply-fcontour-trans c super-reflect-y))])

; FGlyph  (T . ... -> T) . ... -> FGlyph
; apply a geometric transformations to a glyph
(define (apply-fglyph-trans g fn . args)
  (let ([t (lambda (o) (apply fn o args))])
    (struct-copy fglyph g
                 [components (map t (fglyph-components g))]
                 [anchors (map t (fglyph-anchors g))]
                 [contours (map t (fglyph-contours g))])))

; FGlyph Number [Number] -> FGlyph
; apply a geometric transformations to a glyph
(define (fglyph-scale g fx [fy fx])
    (struct-copy fglyph (apply-fglyph-trans g scale fx fy)
                 [advance (list (* (car (fglyph-advance g)) fx)
                                (* (cadr (fglyph-advance g)) fy))]))

; FGlyph -> DrawableGlyph
; produce a drawable glyph
(define (draw-fglyph g)
   (append (list (fglyph-name g)
                 (car (fglyph-advance g)))
           (map fcontour->bezier (fglyph-contours g))))


; FContour -> Bezier
; produce a Bezier curve from an FContour
(define (fcontour->bezier c)
  (fcontour-points c))

; Glyph -> FGlyph
; produce a glyph from a ufo's glyph
(define (glyph->fglyph g)
  (fglyph (glyph-name g)
          (let ((a (glyph-advance g)))
               (list (advance-width a) (advance-height a)))
             (map-contours ufo->contour g)
             (map-components ufo->component g)
             (map-anchors ufo->anchor g)))


; Contour -> FContour
; produce a contour from a ufo contour
(define (ufo->contour c)
  (fcontour (contour-identifier c) (contour->bezier c)))

; Anchor -> FAnchor
; produce an anchor froma a ufo anchor
(define (ufo->anchor a)
  a)
 
; Component -> FComponent
; produce a component from a ufo component
(define (ufo->component c)
  c)

; Font -> (listOf FGlyph)
; produce a sorted list of Glyphs from a Ufo
(define (ufo->fglyphs f)
  (map-glyphs glyph->fglyph f #:sorted #t))

; FGlyph -> Glyph
; produce a ufo glyph from a glyph
(define (fglyph->glyph g [ufo #f]) 
  (match g
    [(fglyph name (list aw ah) contours components anchors)
     (if ufo
         (let ((ufo-glyph (get-glyph ufo name)))
           (struct-copy glyph ufo-glyph
                        [advance (advance aw ah)]
                        [anchors (map anchor->ufo anchors)]
                        [components (map component->ufo components)]
                        [contours (map contour->ufo contours)]))
         (glyph 1 name (advance aw ah) 
                '() #f #f '() 
                (map anchor->ufo anchors) 
                (map component->ufo components) 
                (map contour->ufo contours)))]))

; FAnchor -> Anchor
; produce a ufo anchor from an anchor
(define (anchor->ufo a)
  a)

; FComponent -> Component
; produce a component anchor from a component
(define (component->ufo c)
  c)

; FContour -> Contour
; produces a ufo:contour from a FlatContour
(define (contour->ufo fc)
  (contour (fcontour-identifier fc) 
           (contour-points (bezier->contour (fcontour-points fc)))))
 

; FComponent, FComponent -> FComponent
; produce a new component with scale fields imported from another component
(define (import-component-scale c1 c2)
  (match c1 
    [(component base (trans-mat _ _ _ _ x-offset y-offset) id) 
     (match c2
       [(component _ (trans-mat x-scale xy-scale yx-scale y-scale _ _) _)
        (component base (trans-mat x-scale xy-scale yx-scale y-scale x-offset y-offset) id)])]))

; FFont (listOf Symbol) -> FFont
; produce a font with the glyphs NOT in gs removed
(define (keep-glyphs f gs)
  (struct-copy ffont f
               [glyphs (filter (lambda (g)
                                 (member (fglyph-name g) gs))
                                 (ffont-glyphs f))]))

; FGlyph [(FContour -> FContour)] -> FGlyph
; produce a new glyph with contours sorted
(define (sort-contours g [sort-points-fn identity])
  (struct-copy fglyph g
               [contours 
                (sort (map sort-points-fn (fglyph-contours g))
                      contour<?)]))
      
; FContour FContour -> Boolean
; True if the first contour has fewer points than the second
; or if the first point of the first contours is nearer the origin
; than the first point of the second contour
(define (contour<? c1 c2)
  (let ([l1 (length (fcontour-points c1))]
        [l2 (length (fcontour-points c2))])
    (or (< l1 l2)
        (and (= l1 l2)
             (< (vec-length (car (fcontour-points c1)))
                (vec-length (car (fcontour-points c2))))))))
             
; FContour -> Boolean 
; True if the last point is equal to the first point 
; (and the contour has more than one point)
(define (fcontour-closed? c)
  (let ([cp (fcontour-points c)])
    (and (> (length cp) 1) (closed? cp))))

; FContour -> (listOf Points)
; produce a list of "on curve" points 
; (remove the control points)
(define (on-curve-points c)
  (on-curve-nodes (fcontour-points c)))

; FPoint -> FPoint
; True if x1 < x2, if x coord. are equal true if y1 < y2
(define (point<? pt1 pt2)
  (or (< (vec-x pt1) (vec-x pt2))
      (and (= (vec-x pt1) (vec-x pt2)) 
           (< (vec-y pt1) (vec-y pt2)))))

; FContour -> FContour
; Rotate the point list by removing the first 'segment' and appeding it to the end of contour
(define (cycle-points c)
  (let ([cp (fcontour-points c)])
    (struct-copy fcontour c
                 [points (append (cdddr (take cp (- (length cp) 1)))
                                 (take cp 4))])))

; FContour -> FContour
; Rearrange the contour so that the first point is the mimimum (using point<) of all points
(define (canonical-start-point c)
  (if (fcontour-closed? c)
      (let ((min-pt (car (sort (on-curve-points c) point<?))))
        (letrec ((aux (lambda (c)
                        (let ([pts (fcontour-points c)])
                          (if (equal? (car pts) min-pt) c
                              (aux (cycle-points c)))))))
          (aux c)))
      c))

; FContour -> FContour
; reverse the contour
(define (freverse-contour c)
  (struct-copy fcontour c [points (reverse (fcontour-points c))]))

; FGlyph -> FGlyph
; produce a new glyph trying to fix the directions of contours
(define (fcorrect-directions g)
  (define (area cs)
    (foldl (lambda (c acc) (+ acc (signed-polygonal-area (fcontour->bezier c))))
           0 cs))
  (let ([cs (fglyph-contours g)])
    (struct-copy fglyph g
                 [contours (if (< (area cs) 0)
                               (map freverse-contour cs)
                               cs)])))

; FComponent -> Point
; Produce a Point from x and y offset
(define (component->pt c)
  (pt (trans-mat-x-offset (component-matrix c))
       (trans-mat-y-offset (component-matrix c))))


; FGlyph -> FGlyph
; produce a new glyph with components sorted
(define (sort-components g)
  (struct-copy fglyph g
               [components (sort (fglyph-components g)
                                 (lambda (c1 c2)
                                   (or (string<? (symbol->string (component-base c1))
                                                 (symbol->string (component-base c2)))
                                       (point<? (component->pt c1) (component->pt c2)))))]))

; FGlyph -> FGlyph
; produce a new glyph with anchors sorted
(define (sort-anchors g)
  (struct-copy fglyph g
               [anchors (sort (fglyph-anchors g)
                              #:key anchor-name
                              string<?)]))

; FFont [Boolean] -> FFont
; Maximize the 'interpolability' of a font (or, at least, that's what we hope to do)
(define (prepare-for-interpolation f [weak #t])
    (struct-copy ffont f
                 [glyphs (map (lambda (g) (prepare-glyph g weak)) 
                              (ffont-glyphs f))]))

; FGlyph [Boolean] -> FGlyph
; Maximize the 'interpolability' of a glyph
; if weak is false, correct directions and reset the first points of contours
(define (prepare-glyph g [weak #t])
  (sort-components 
   (sort-anchors
    (sort-contours (if weak g (fcorrect-directions g)) (if weak identity canonical-start-point)))))

; Font Font -> Font Font
; produce two new interpolable fonts 
(define (compatible-fonts f1 f2)
  (let* ((common (set->list 
                  (set-intersect
                   (list->set (map fglyph-name (ffont-glyphs f1)))
                   (list->set (map fglyph-name (ffont-glyphs f2))))))
         (fc1 (keep-glyphs f1 common))
         (fc2 (keep-glyphs f2 common))
         (compatible-glyphs (filter
                             identity
                             (map compatible-glyphs 
                                 (ffont-glyphs fc1)
                                 (ffont-glyphs fc2))))
         (kerning (compatible-kerning
                   (ffont-kerning f1) 
                   (ffont-kerning f2)))
         (infos (compatible-infos
                 (ffont-info f1) 
                 (ffont-info f2))))
    (values (struct-copy ffont fc1
                         [glyphs (map car compatible-glyphs)]
                         [kerning (car kerning)]
                         [info (car infos)])
            (struct-copy ffont fc2
                         [glyphs (map cdr compatible-glyphs)]
                         [kerning (cdr kerning)]
                         [info (cdr infos)]))))

; FKerning FKerning -> (FKerning . FKernig)
; produce a pair of Kerning with the same pairs
(define (compatible-kerning k1 k2)
  (let [(pairs (common-pairs k1 k2))]
    (cons (filter-kerning k1 pairs)
          (filter-kerning k2 pairs))))

; FKerning FKerning -> (listOf (Symbol . Symbol))
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

; FKerning (listOf (Symbol . Symbol)) -> FKerning
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

; FInfo FInfo -> (FInfo . FInfo)
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

                
; FGlyph FGlyph -> (FGlyph . FGlyph) or False
; if the two Glyph can be made interpolable produce a pair of Glyph, otherwise False
(define (compatible-glyphs g1 g2)
  (let ((c1 (fglyph-contours g1))
        (c2 (fglyph-contours g2)))
    (if (and (= (length c1) (length c2))
             (andmap (lambda (a b) (= (length (fcontour-points a)) (length (fcontour-points b))))
                     c1 c2))
        (let-values (((c1 c2) (compatible-components (fglyph-components g1)
                                                     (fglyph-components g2)))
                     ((a1 a2) (compatible-anchors (fglyph-anchors g1)
                                                  (fglyph-anchors g2))))
          (cons (struct-copy fglyph g1 [components c1] [anchors a1])
                (struct-copy fglyph g2 [components c2] [anchors a2])))
        #f)))


; (listOf FContour) (listOf FContour) -> (listOf FContour)
; match the second list of contours against the first list.
(define (match-contour-order cs1 cs2)
  (define (contour-distance c1 c2)
    (foldl (lambda (p1 p2 d)
             (+ d (vec-length (vec- p2 p1))))
           0 (fcontour-points c1) (fcontour-points c2)))
  (reverse
   (cdr 
    (foldl (lambda (cm acc)
             (let* ([r (cdr acc)]
                    [cs (car acc)]
                    [m (car (argmin cdr (filter (lambda (i) (identity (cdr i)))
                                                (map (lambda (c)
                                                       (cons c
                                                             (if (= (length (fcontour-points c)) (length (fcontour-points cm)))
                                                                    (contour-distance cm c)
                                                                    #f)))
                                                     cs))))])
               (cons (remq m cs) (cons m r)))) 
           (cons cs2 '())
           cs1))))


; FGlyph FGlyph -> FGlyph
; match the contours of the second glyph against the first
(define (match-glyphs-contours g1 g2)
  (let ([cs1 (fglyph-contours g1)]
        [cs2 (fglyph-contours g2)])
    (struct-copy fglyph g2 [contours (match-contour-order cs1 cs2)])))

             

; FFont FFont -> FFont
; match the contours of the second font against the first
(define (match-fonts-contours f1 f2)
  (let ([gs1 (ffont-glyphs f1)]
        [gs2 (ffont-glyphs f2)])
    (struct-copy ffont f2 [glyphs (map match-glyphs-contours gs1 gs2)])))
                  
             
; (listOf FComponent) (listOf FComponent) -> (listOf FComponent) (listOf FComponent)
; produce two interpolable component lists
(define (compatible-components c1 c2)
  (let ((common-comp (set->list 
                      (set-intersect (list->set (map component-base c1))
                                     (list->set (map component-base c2))))))
         (values (filter-common common-comp c1 component-base)
                 (filter-common common-comp c2 component-base))))

; (listOf FAnchor) (listOf FAnchor) -> (listOf FAnchor) (listOf FAnchor)
; produce two interpolable anchor lists
(define (compatible-anchors a1 a2)
  (let ((common-anc (set->list 
                     (set-intersect (list->set (map anchor-name a1))
                                    (list->set (map anchor-name a2))))))
         (values (filter-common common-anc a1 anchor-name)
                 (filter-common common-anc a2 anchor-name))))

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


; FFont FGlyph Symbol -> FGlyph
; decompose glyph components to outlines
(define (decompose-fglyph f g)
  (let* ([cs (fglyph-components g)])
    (if (null? cs)
        g
        (let* ([bases (map (lambda (g) (decompose-fglyph f g))
                           (fget-glyphs f (map component-base cs)))]
               [dcs (apply append (map fcomponent->outlines cs bases))])
          (struct-copy fglyph g
                       [components null]
                       [contours (append (fglyph-contours g) dcs)])))))

; Component Glyph -> (listOf Contour)
; reduce the component to a list of contour
(define (fcomponent->outlines c b)
  
  (let ([m (component-matrix c)]
        [base-contours (fglyph-contours b)])
    (map (lambda (c) (transform c m))
         base-contours)))