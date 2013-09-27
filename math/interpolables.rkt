#lang racket

(require "../ufo.rkt"
         "../fontpict.rkt"
         "../geometry.rkt")


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

; Font Boolean -> Font
(define (prepare-font f [weak #t] [auto-directions #f])
  (struct-copy font (if auto-directions (correct-directions f) f)
               [fontinfo (prepare-info (font-fontinfo f))]
               [kerning (prepare-kerning (font-kerning f))]
               [layers (list (prepare-layer (get-layer f) weak))]))

; FontInfo -> Fontinfo
(define (prepare-info i)       
  (let* ([keys (map car *infos*)]
         [psname (dict-ref i 'postscriptFontName "untitled")]
         [famname (dict-ref i 'familyName "untitled")])
    (hash-set*
     (make-immutable-hash
      (filter identity
              (dict-map i (lambda (key value)
                            (if (member key keys)
                                (cons key value)
                                #f)))))
     'postscriptFontName psname 
     'familyName famname)))

; Kerning -> Kerning
(define (prepare-kerning k)    ; stub
  k)

; Layer Boolean -> Layer
(define (prepare-layer l [weak #t])
  (struct-copy layer l
               [glyphs (map-glyphs (lambda (g) (prepare-glyph g weak)) l)]))

; Glyph Boolean -> Glyph
(define (prepare-glyph g [weak #t])
  (struct-copy glyph g
               [anchors (sort-anchors (glyph-anchors g))]
               [components (sort-components (glyph-components g))]
               [guidelines '()]
               [contours (sort-contours 
                          (map-contours (lambda (c) (prepare-contour c weak)) g))]))

; (listof Anchor) -> (listof Anchor)
(define (sort-anchors loa)
  (sort loa #:key anchor-name string<?))

; (listof Component) -> (listof Component)
(define (sort-components loc)
  (sort loc (lambda (c1 c2)
              (or (string<? (symbol->string (component-base c1))
                            (symbol->string (component-base c2)))
                  (pos<? (component-pos c1) (component-pos c2))))))

; (listof Contour) -> (listof Contour)
(define (sort-contours loc)
  (sort loc contour<?))

; Contour Boolean -> Contour
(define (prepare-contour c [weak #t])    
  (let ([ci (remove-line-segments c)])
    (if weak ci (struct-copy contour ci
                             [points (sort-points (contour-points ci))]))))

; Contour -> Contour
; transform every segment in a curve segment
(define (remove-line-segments c)
  (letrec ([aux (lambda (vs)
                  (match vs
                    [(list) '()]
                    [(list-rest v1 v2 v3 r)
                     (append (list (point v1 'offcurve #f #f #f)
                                   (point v2 'offcurve #f #f #f)
                                   (point v3 'curve #f #f #f))
                             (aux r))]))])
  (let* ([bz (contour->bezier c)]
         [rpts (aux (cdr bz))])
    (struct-copy contour c
                 [points (if (contour-open? c)
                             (cons (point (car bz) 'move #f #f #f) rpts)
                             (cons (point (car bz) 'curve #f #f #f)
                                   (drop-right rpts 1)))]))))

; Font Font -> Font Font
(define (compatible-fonts f1 f2)
  (let*-values ([(i1 i2) (compatible-infos    (font-fontinfo f1) 
                                              (font-fontinfo f2))]
                [(l1 l2) (compatible-layers   (get-layer f1)
                                              (get-layer f2))]
                [(g1 g2) (compatible-groups   (font-groups f1) l1
                                              (font-groups f2) l2)]
                [(k1 k2) (compatible-kernings (font-kerning f1) l1 g1
                                              (font-kerning f2) l2 g2)])
    (values
     (struct-copy font f1
                  [fontinfo i1]
                  [kerning  k1]
                  [groups   g1]
                  [layers   l1])
     (struct-copy font f2
                  [fontinfo i2]
                  [kerning  k2]
                  [groups   g2]
                  [layers   l2]))))

; FontInfo FontInfo -> FontInfo FontInfo 
(define (compatible-infos i1 i2) 
  (let ([loi1 (sort-by-key (hash->list i1))]
        [loi2 (sort-by-key (hash->list i2))])
  (values i1 i2)))

; Layer Layer -> Layer Layer 
(define (compatible-layers l1 l2)                ;stub
  (values l1 l2))
                
; Groups Groups -> Groups Groups 
(define (compatible-groups g1 l1 g2 l2)          ;stub
  (values g1 g2))

; Kerning Kerning -> Kerning Kerning 
(define (compatible-kernings k1 l1 g1 k2 l2 g2)  ;stub
  (values k1 k2))




; (listof Point) -> (listof Point)
(define (sort-points lop)                     
  (if (eq? (point-type (car lop)) 'move)
      lop
      (let ([min-pt (minimum-curve-point lop)])
        (letrec ([rotate-until
                  (lambda (lop)
                    (if (vec= min-pt (point-pos (car lop)))
                        lop
                        (rotate-until (append (drop lop 3) (take lop 3)))))])
          (rotate-until lop)))))

; (listof Point) -> Vec
(define (minimum-curve-point lop)
  (car (sort (map point-pos 
                  (filter (lambda (p) (eq? 'curve (point-type p))) 
                          lop)) 
             pos<?)))
        

; Component -> Vec
; Produce a Vec from x and y offset
(define (component-pos c)
  (vec (trans-mat-x-offset (component-matrix c))
       (trans-mat-y-offset (component-matrix c))))

; Vec -> Vec
; True if x1 < x2, if x coord. are equal true if y1 < y2
(define (pos<? v1 v2)
  (or (< (vec-x v1) (vec-x v2))
      (and (= (vec-x v1) (vec-x v2)) 
           (< (vec-y v1) (vec-y v2)))))

; Contour Contour -> Boolean
; True if the first contour is open and the second closed
; otherwise:
; True if the first contour has fewer points than the second
; or if the first point of the first contours is nearer the origin
; than the first point of the second contour
(define (contour<? c1 c2)
  (let* ([p1 (contour-points c1)]
         [p2 (contour-points c2)]
         [l1 (length p1)]
         [l2 (length p2)])
    (or (and (contour-open? c1) (not (contour-open? c2)))
        (and (and (contour-open? c1) (contour-open? c2))
             (or
              (< l1 l2)
              (and (= l1 l2)
                   (< (vec-length (point-pos (car p1)))
                      (vec-length (point-pos (car p2))))))))))


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

; AssociationList -> AssociationList
; sort the alist with key name
(define (sort-by-key alist)
  (sort alist string<? 
        #:key (lambda (p) 
                (symbol->string (car p)))))

; Symbol Symbol -> Boolean
(define (symbol<? s1 s2)
  (string<? (symbol->string s1)
            (symbol->string s2)))