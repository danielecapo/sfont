#lang racket

(require "../../main.rkt"
         "../../geometry.rkt")


(provide 
 (contract-out
  [interpolable-fonts (->* (font? font?) (boolean? boolean?) (values font? font?))]
  [prepare-font (->* (font?) (boolean? boolean?) font?)]
  [prepare-info (-> fontinfo/c fontinfo/c)]
  [prepare-kerning (-> kerning/c kerning/c)]
  [prepare-layer (->* (layer?) (boolean?) layer?)]
  [prepare-glyph (->* (glyph?) (boolean?) glyph?)]
  [prepare-contour (->* (contour?) (boolean?) contour?)]
  [compatible-fonts (-> font? font? (values font? font?))]
  [compatible-glyphs (-> glyph? glyph? (values glyph? glyph?))]
  [compatible-layers (-> layer? layer? (values layer? layer?))]
  [compatible-infos (-> fontinfo/c fontinfo/c (values fontinfo/c fontinfo/c))]
  [compatible-groups (-> groups/c groups/c (listof symbol?) (values groups/c groups/c))]
  [compatible-kernings (-> kerning/c kerning/c (listof symbol?) (listof symbol?) (values kerning/c kerning/c))]
  [match-fonts-contours (-> font? font? font?)]
  [import-component-scale (-> component? component? component?)]))
  
  


; Font Font Boolean Boolean -> Font
(define (interpolable-fonts f1 f2 [weak #t] [auto-directions #f])
  (compatible-fonts (prepare-font f1 weak auto-directions)
                    (prepare-font f2 weak auto-directions)))

 
; Font Boolean Boolean -> Font
(define (prepare-font f [weak #t] [auto-directions #f])
  (struct-copy font (if auto-directions (correct-directions f) f)
               [layers (list (hash-ref (font-layers f) foreground))]
               [fontinfo (prepare-info (font-fontinfo f))]
               [kerning (prepare-kerning (font-kerning f))]
               [glyphs (map-glyphs (curryr prepare-glyph weak) f)]))

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
(define (prepare-kerning k)    
  k)

; Layer Boolean -> Layer
(define (prepare-layer l [weak #t])
  (struct-copy layer l
               [anchors (sort-anchors (layer-anchors l))]
               [components (sort-components (layer-components l))]
               [guidelines '()]
               [contours (sort-contours 
                          (map-contours (lambda (c) (prepare-contour c weak)) l))]))

; Glyph Boolean -> Glyph
(define (prepare-glyph g [weak #t])
  (struct-copy glyph g
               [layers (list (prepare-layer (get-layer g foreground)))]))

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
  (let*-values ([(i1 i2)   (compatible-infos       (font-fontinfo f1) 
                                                   (font-fontinfo f2))]
                [(gl1 gl2) (compatible-font-glyphs f1
                                                   f2)]
                [(g1 g2)   (compatible-groups      (font-groups f1)
                                                   (font-groups f2) 
                                                   (map glyph-name gl1))]
                [(k1 k2)   (compatible-kernings    (font-kerning f1)
                                                   (font-kerning f2)
                                                   (map glyph-name gl1)
                                                   (hash-keys g1))])
    (values
     (struct-copy font f1
                  [fontinfo i1]
                  [kerning  k1]
                  [groups   g1]
                  [glyphs   gl1])
     (struct-copy font f2
                  [fontinfo i2]
                  [kerning  k2]
                  [groups   g2]
                  [glyphs   gl2]))))

; FontInfo FontInfo -> FontInfo FontInfo 
(define (compatible-infos i1 i2) 
  (let ([loi1 (sort-by-key (hash->list i1))]
        [loi2 (sort-by-key (hash->list i2))])
    (let-values ([(li1 li2) (commons loi1 loi2 car eq? symbol<?)])
      (values (make-immutable-hash li1)
              (make-immutable-hash li2)))))

; Font Font -> (listof Glyph) (listof Glyph)
(define (compatible-font-glyphs f1 f2)
  (let*-values ([(gs1 gs2) (commons (map-glyphs identity f1 #:sorted #t)
                                    (map-glyphs identity f2 #:sorted #t)
                                    glyph-name eq? symbol<?)])
    (compatible-list-of-glyphs gs1 gs2)))

; (listof Glyph) (listof Glyph) -> (listof Glyph) (listof Glyph)
(define (compatible-list-of-glyphs log1 log2)
  (let ([cl (map (lambda (g1 g2)
                   (let-values ([(ng1 ng2) (compatible-glyphs g1 g2)])
                     (cons g1 g2)))
                 log1 
                 log2)])
    (values (filter identity (map car cl))
            (filter identity (map cdr cl)))))

; Glyph Glyph -> Glyph or False Glyph or False
(define (compatible-glyphs g1 g2)
  (let-values ([(l1 l2) (compatible-layers (get-layer g1 foreground)
                                           (get-layer g2 foreground))])
    (if (and l1 l2)
        (values (struct-copy glyph g1 [layers (list l1)])
                (struct-copy glyph g2 [layers (list l2)]))
        (values #f #f))))

; Layer Layer -> Layer or False Layer or False
(define (compatible-layers l1 l2)
  (let ([cs1 (layer-contours l1)]
        [cs2 (layer-contours l2)])
    (cond [(not (= (length cs1) (length cs2))) (values #f #f)]
          [(andmap (lambda (a b) (= (length (contour-points a)) (length (contour-points b))))
                     cs1 cs2)
           (values #f #f)]
          [else (let-values (((c1 c2) (compatible-components (layer-components l1)
                                                             (layer-components l2)))
                             ((a1 a2) (compatible-anchors    (layer-anchors l1)
                                                             (layer-anchors l2))))
                  (values (struct-copy layer l1 [components c1] [anchors a1])
                          (struct-copy layer l2 [components c2] [anchors a2])))])))

; (listof Component) (listof Component) -> (listof Component) (listof Component)
(define (compatible-components loc1 loc2)
  (commons loc1 loc2 component-base eq? symbol<?))

; (listof Anchor) (listof Anchor) -> (listOf Anchor) (listof Anchor)
; produce two interpolable anchor lists
(define (compatible-anchors loa1 loa2)
  (commons loa1 loa2 anchor-name string=? string<?))


                
; Groups Groups -> Groups Groups 
(define (compatible-groups g1 g2 gs)
  (let* ([ng1 (purge-groups g1 gs)]
         [ng2 (purge-groups g2 gs)])
    (common-groups ng1 ng2)))


; Groups (listof Symbol) -> Groups
(define (purge-groups gs los)
  (let ([gl (hash->list gs)])
    (make-immutable-hash
     (filter (lambda (g) (not (null? (cdr g))))
             (map (lambda (g)
                   (cons (car g)
                         (filter (lambda (s) (member s los))
                                 (cdr g))))
                  gl)))))

; Groups Groups -> Groups Groups
(define (common-groups g1 g2)
  (let-values ([(ng1 ng2) (commons (sort-by-key (hash->list g1))
                                   (sort-by-key (hash->list g2))
                                   car eq? symbol<?)])
    (let ([g (make-immutable-hash
              (filter (lambda (g) (not (null? (cdr g))))
                      (map (lambda (g1 g2)
                             (cons (car g1)
                                   (set->list
                                    (set-intersect (list->set (cdr g1))
                                                   (list->set (cdr g2))))))
                           ng1 ng2)))])
      (values g g))))
    
    


; Kerning Kerning (listof Symbol) (listof Symbol) -> Kerning Kerning 
(define (compatible-kernings k1 k2 lgl lgr)
  (let ([los (append lgl lgr)])
    (let ([nk1 (purge-kerns k1 los)]
          [nk2 (purge-kerns k2 los)])
      (common-kerns nk1 nk2))))

; Kerning (listof Symbol) -> Kerning
(define (purge-kerns k los)
  (let ([k (hash-map k (lambda (key val) (cons key (hash->list val))))])
    (make-immutable-hash
     (filter (lambda (kg) (member (car kg) los))
             (map (lambda (kg)
                    (cons (car kg)
                          (make-immutable-hash
                           (filter (lambda (v) (member (car v) los)) (cdr kg)))))
                  k)))))

(define (common-kerns k1 k2)
  (let [(pairs (common-pairs k1 k2))]
    (values (filter-kerning k1 pairs)
            (filter-kerning k2 pairs))))

; Kerning Kerning -> (listOf (Symbol . Symbol))
; produce a list of kerning pairs used in noth fonts
(define (common-pairs k1 k2)
  (let ([k1 (hash-map k1 (lambda (key val) (cons key (hash->list val))))]
        [k2 (hash-map k2 (lambda (key val) (cons key (hash->list val))))])
    (letrec ([kerning-pairs (lambda (k)
                              (foldl (lambda (k acc)
                                       (append acc
                                               (map (lambda (r) 
                                                      (cons (car k) (car r))) 
                                                    (cdr k))))
                                     '()
                                     k))])
      (set->list (set-intersect (list->set (kerning-pairs k1))
                                (list->set (kerning-pairs k2)))))))

; (listof (Symbol . (listof Symbol Real))) (listOf (Symbol . Symbol)) -> Kerning
; produce a new kerning removing data for combinations NOT in pairs
(define (filter-kerning k1 pairs)
  (define (filter-left k1 left)
    (filter (lambda (k) (member (car k) left)) k1))
  (let ([k1 (hash-map k1 (lambda (key val) (cons key (hash->list val))))])
    (make-immutable-hash
     (map (lambda (l)
            (cons (car l)
                  (make-immutable-hash
                   (filter (lambda (r)
                             (member (cons (car l) (car r)) pairs))
                           (cdr l)))))
          (filter-left k1 (map car pairs))))))

        
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


; Font Font -> Font
; match the contours of the second font against the first
(define (match-fonts-contours f1 f2)
  (let ([gs1 (map-glyphs identity f1)]
        [gs2 (map-glyphs identity f2)])
    (struct-copy font f2 
                 [glyphs (map match-glyphs-contours gs1 gs2)])))

; Glyph Glyph -> Glyph
; match the contours of the second glyph against the first
(define (match-glyphs-contours g1 g2)
  (let ([cs1 (layer-contours (get-layer g1 foreground))]
        [cs2 (layer-contours (get-layer g2 foreground))])
    (struct-copy glyph g2 
                 [layers 
                  (list (struct-copy layer (get-layer g2 foreground)
                                     [contours (match-contour-order cs1 cs2)]))])))

; (listof Contour) (listof Contour) -> (listof Contour)
; match the second list of contours against the first list.
(define (match-contour-order cs1 cs2)
  (define (contour-distance c1 c2)
    (foldl (lambda (p1 p2 d)
             (+ d (vec-length (vec- p2 p1))))
           0 (map-points point-pos c1) (map-points point-pos c2)))
  (reverse
   (cdr 
    (foldl (lambda (cm acc)
             (let* ([r (cdr acc)]
                    [cs (car acc)]
                    [m (car (argmin cdr (filter (lambda (i) (identity (cdr i)))
                                                (map (lambda (c)
                                                       (cons c
                                                             (if (= (length (contour-points c)) (length (contour-points cm)))
                                                                    (contour-distance cm c)
                                                                    #f)))
                                                     cs))))])
               (cons (remq m cs) (cons m r)))) 
           (cons cs2 '())
           cs1))))
        
; Component, Component -> Component
; produce a new component with scale fields imported from another component
(define (import-component-scale c1 c2)
  (match c1 
    [(component base (trans-mat _ _ _ _ x-offset y-offset) id) 
     (match c2
       [(component _ (trans-mat x-scale xy-scale yx-scale y-scale _ _) _)
        (component base (trans-mat x-scale xy-scale yx-scale y-scale x-offset y-offset) id)])]))

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

; (listof A) (listof A) (A -> B) (B B -> Boolean) (B B -> Boolean) -> (listof A) (listof A)
(define (commons l1 l2 key k=? k<?)
  (if (or (null? l1) (null? l2))
      (values '() '())
      (let* ([f1 (car l1)]
             [f2 (car l2)]
             [k1 (key f1)]
             [k2 (key f2)])
        (cond [(k=? k1 k2)
               (let-values ([(r1 r2) (commons (cdr l1) (cdr l2) key k=? k<?)])
                 (values (cons f1 r1) (cons f2 r2)))]
              [(k<? k1 k2)
               (let-values ([(r1 r2) (commons (cdr l1) l2 key k=? k<?)])
                 (values r1 r2))]
              [else
               (let-values ([(r1 r2) (commons l1 (cdr l2) key k=? k<?)])
                 (values r1 r2))]))))
            