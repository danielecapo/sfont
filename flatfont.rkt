#lang racket

(require (prefix-in ufo: "font.rkt")
         (prefix-in ufo: "glif.rkt")
         "vec.rkt"
         "fontpict.rkt"
         (prefix-in bz: "bezier.rkt")
         slideshow/pict-convert
         (planet wmfarr/plt-linalg:1:13/matrix))

(provide (all-defined-out))

(struct font (ufo info kerning glyphs) 
  #:transparent
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (font-info f) 'ascender 750)]
          [descender (dict-ref (font-info f) 'descender -250)]
          [glyphs (map (lambda (g) (draw-glyph (decompose-glyph f g)))
                       (get-glyphs f *text*))])
      (apply pictf:font ascender descender glyphs))))


(define (get-glyphs f gs)
  (let ([g-hash (make-hash (map (lambda (g) 
                                  (cons (glyph-name g) g)) 
                                (font-glyphs f)))])
    (filter identity
            (map (lambda (g) (hash-ref g-hash g #f))
                 gs))))
    
(define (flatfont:ufo->font f)
  (font f
        (interpolable-infos (sort-by-keyword (hash->list (ufo:font-fontinfo f))))
        (if (ufo:font-kerning f)
            (sort-by-keyword 
             (hash-map (ufo:font-kerning f)
                       (lambda (left right)
                         (cons left (sort-by-keyword (hash->list right))))))
            '())
        (flat-glyphs f)))

(define (sort-by-keyword alist)
  (sort alist string<? 
        #:key (lambda (p) 
                (symbol->string (car p)))))

(define (flatfont:font->ufo f)
  (struct-copy ufo:font (font-ufo f)
               [fontinfo (ufo-infos f)]
               [kerning (if (null? (font-kerning f))
                            #f
                            (make-immutable-hash
                             (map (lambda (p) (cons (car p) (make-immutable-hash (cdr p))))
                                  (font-kerning f))))]
               [layers (list (struct-copy ufo:layer (ufo:get-layer (font-ufo f))
                                          [glyphs (map (lambda (g) (glyph->ufo g (font-ufo f)))  
                                                       (font-glyphs f))]))]))


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

(define (interpolable-infos infos)
  (let ((keys (map car *infos*)))
    (filter identity
            (dict-map infos (lambda (key value)
                              (if (member key keys)
                                  (cons key value)
                                  #f))))))

(define (ufo-infos f)
  (let ((infos (make-immutable-hash 
                (dict-map
                 (font-info f)
                 (lambda (key value)
                   (cons key ((car (dict-ref *infos* key)) value))))))
        (psname (dict-ref (ufo:font-fontinfo (font-ufo f)) 'postscriptFontName "untitled"))
        (famname (dict-ref (ufo:font-fontinfo (font-ufo f)) 'familyName "untitled")))
    (dict-set* infos 'postscriptFontName psname 'familyName famname)))

     
     
     
(define (pt x y) (list x y))

(struct glyph (name advance contours components anchors)
  #:transparent)

(define (draw-glyph g)
   (append (list (glyph-name g)
                 (car (glyph-advance g)))
           (map contour->bezier (glyph-contours g))))

; contour->bezier
; Contour -> Bezier
; produce a Bezier curve from a Contour

(define (contour->bezier c)
  (map list->vec c))

;           (map (lambda (c)
;                  (letrec ((aux (lambda (pts acc)
;                                 
;                                  (match pts
;                                    [(list-rest off1 off2 p rp)
;                                     (aux rp (append acc (list (cons 'off off1) (cons 'off off2) p)))]
;                                    [(list) acc]))))
;                    (cons (cons 'move (car c))
;                          (aux (cdr c) '()))))
;                (glyph-contours g))))
  

(define (flatfont:glif->glyph g)
  (glyph (ufo:glyph-name g)
             (let ((a (ufo:glyph-advance g)))
               (pt (ufo:advance-width a) (ufo:advance-height a)))
             (ufo:map-contours flat-contour g)
             (ufo:map-components flat-component g)
             (ufo:map-anchors flat-anchor g)))

; flat-contour
; ufo:contour -> FlatContour
; produce a flat contour from a ufo:contour

(define (flat-contour c)
  (map vec->list (ufo:contour->bezier c)))

#;
(define (flat-contour c)
  (letrec ((ensure-first-on-curve 
            (lambda (pts)
              (match pts
                [(list-rest (ufo:point _ _ 'move _ _ _) pr) pts]
                [(list-rest (ufo:point _ _ 'curve _ _ _) pr) pts]
                [(list-rest (ufo:point _ _ 'line _ _ _) pr) pts]
                [(list-rest (ufo:point _ _ 'qcurve _ _ _) pr) pts]
                [(list-rest (ufo:point _ _ 'offcurve _ _ _) pr) 
                 (ensure-first-on-curve (append pr (list (car pts))))])))
           (flattener 
            (lambda (pts acc)
              (match pts
                [(list-rest (or
                             (ufo:point x y 'curve _ _ _)
                             (ufo:point x y 'move _ _ _)
                             (ufo:point x y 'line _ _ _))
                            (ufo:point x1 y1 'line _ _ _)
                            _)
                 (flattener (cdr pts) (append acc (list (pt x y) (pt x y)(pt x1 y1))))]
                [(list-rest (ufo:point x y 'offcurve _ _ _) pr)
                 (flattener pr (append acc (list (pt x y))))]
                [(list-rest (ufo:point x y 'curve _ _ _) pr)
                 (flattener pr (append acc (list (pt x y))))]
                [(list-rest (ufo:point x y 'move _ _ _) pr)
                 (flattener pr (append acc (list (pt x y))))]
                [(list-rest (ufo:point x y 'line _ _ _) pr)
                 (flattener pr (append acc (list (pt x y))))]
                [(list) acc]))))
    (let* ((points (ensure-first-on-curve (ufo:contour-points c)))
           (first-point (car points)))
      (if (eq? (ufo:point-type first-point) 'move)
          (flattener points '())
          (flattener (append points (list first-point)) '())))))
             
                  
           
(define (flat-anchor a)
  (list (ufo:anchor-name a) (ufo:anchor-x a) (ufo:anchor-y a)))
  
(define (flat-component c)
  (match c
    [(ufo:component base x-scale xy-scale yx-scale y-scale x-offset y-offset _)
     (list base x-scale xy-scale yx-scale y-scale x-offset y-offset)]))

(define (flat-glyphs f)
  (ufo:map-glyphs flatfont:glif->glyph f #:sorted #t))

(define (glyph->ufo g ufo) 
  (match g
    [(glyph name (list aw ah) contours components anchors)
     (let ((ufo-glyph (ufo:get-glyph ufo name)))
       (struct-copy ufo:glyph ufo-glyph
                    [advance (ufo:advance aw ah)]
                    [anchors (map ufo-anchor anchors)]
                    [components (map ufo-component components)]
                    [contours (map ufo-contour contours)]))]))

(define (ufo-anchor a)
  (match a
   [(list name x y) (ufo:anchor x y name #f #f)]))

(define (ufo-component c)
  (match c
   [(list base x-scale xy-scale yx-scale y-scale x-offset y-offset) 
    (ufo:component base x-scale xy-scale yx-scale 
                        y-scale x-offset y-offset #f)]))
; ufo-contour
; FlatContour -> ufo:contour
; produces a ufo:contour from a FlatContour

(define (ufo-contour fc)
  (ufo:bezier->contour (map list->vec fc)))

; import-component-scale
; Component, Component -> Component
; produce a new component with scale fields imported from another component

(define (import-component-scale c1 c2)
  (match c1 
    [(list base _ _ _ _ x-offset y-offset)
     (match c2
       [(list _ x-scale xy-scale yx-scale y-scale _ _)
        (list base x-scale xy-scale yx-scale y-scale x-offset y-offset)])]))

#;
(define (ufo-contour c)
  (letrec ((aux 
            (lambda (prev pts acc)
              (match (cons prev pts)
                [(list-rest `(,x ,y) `(,x ,y) `(,x2 ,y2) `(,x2 ,y2) rest-pts)
                   (aux (pt x2 y2) rest-pts (append acc (list (ufo:make-point #:x x2 #:y y2 #:type 'line))))]
                [(list-rest `(,x ,y) `(,ox1 ,oy1) `(,ox2 ,oy2) `(,x2 ,y2) rest-pts)
                 (aux (pt x2 y2) rest-pts (append acc
                                                  (list (ufo:make-point #:x ox1 #:y oy1)
                                                        (ufo:make-point #:x ox2 #:y oy2)
                                                        (ufo:make-point #:x x2 #:y y2 #:type 'curve))))]
                [(list _) acc]
                [(list) null]))))
    (let* ((first-pt (car c))
           (ufo-pts (aux first-pt (cdr c) null)))
      (ufo:make-contour #:points 
                        (if (closed? c) ufo-pts
                            (cons (ufo:make-point #:x (car first-pt)
                                                  #:y (cadr first-pt)
                                                  #:type 'move)
                                  ufo-pts))))))



(define (keep-glyphs f glyphs)
  (struct-copy font f
               [glyphs (filter (lambda (g)
                                 (member (glyph-name g) glyphs))
                                 (font-glyphs f))]))


(define (sort-contours g [sort-points-fn identity])
  (struct-copy glyph g
               [contours 
                (sort (map sort-points-fn (glyph-contours g))
                      contour<?)]))
                               
(define (contour<? c1 c2)
  (let ([l1 (length c1)]
        [l2 (length c2)])
    (or (< l1 l2)
        (and (= l1 l2)
             (< (vec-length (list->vec (car c1)))
                (vec-length (list->vec (car c2))))))))
             

(define (closed? c)
  (and (> (length c) 1) (equal? (first c) (last c))))

(define (on-curve-points c)
  (if (null? (cdr c)) c
      (cons (car c) (on-curve-points (cdddr c)))))

(define (point<? pt1 pt2)
  (or (< (car pt1) (car pt2))
      (and (= (car pt1) (car pt2)) 
           (< (cadr pt1) (cadr pt2)))))

(define (cycle-points c)
  (append (cdddr (take c (- (length c) 1)))
          (take c 4)))

(define (canonical-start-point c)
  (if (closed? c)
      (let ((min-pt (car (sort (on-curve-points c) point<?))))
        (letrec ((aux (lambda (pts)
                        (if (equal? (car pts) min-pt) pts
                            (aux (cycle-points pts))))))
          (aux c)))
      c))

(define (correct-directions g)
  (define (area cs)
    (foldl (lambda (c acc) (+ acc (signed-polygonal-area (map list->vec c))))
           0 cs))
  (let ([cs (glyph-contours g)])
    (struct-copy glyph g
                 [contours (if (< (area cs) 0)
                               (map reverse cs)
                               cs)])))

(define (sort-components g)
  (struct-copy glyph g
               [components (sort (glyph-components g)
                                 (lambda (c1 c2)
                                   (or (string<? (symbol->string (car c1))
                                                 (symbol->string (car c2)))
                                       (point<? (take-right c1 2) (take-right c2 2)))))]))

(define (sort-anchors g)
  (struct-copy glyph g
               [anchors (sort (glyph-anchors g)
                              #:key car
                              string<?)]))

                                                                                      
(define (prepare-for-interpolation f [weak #t])
    (struct-copy font f
                 [glyphs (map (lambda (g) (prepare-glyph g weak)) 
                              (font-glyphs f))]))

(define (prepare-glyph g [weak #t])
  (sort-components 
   (sort-anchors
    (sort-contours (if weak g (correct-directions g)) (if weak identity canonical-start-point)))))

  
       
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

(define (compatible-kerning k1 k2)
  (let [(pairs (common-pairs k1 k2))]
    (cons (filter-kerning k1 pairs)
          (filter-kerning k2 pairs))))


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

(define (filter-kerning k1 pairs)
  (define (filter-left k1 left)
    (filter (lambda (k) (member (car k) left)) k1))
  (map (lambda (l)
         (cons (car l)
               (filter (lambda (r)
                         (member (cons (car l) (car r)) pairs))
                       (cdr l))))
       (filter-left k1 (map car pairs))))

(define (compatible-infos i1 i2)
  (let* ([common (set->list
                  (set-intersect (list->set (map car i1))
                                 (list->set (map car i2))))]
         [commonf (filter (lambda (i)
                            (if (list? (dict-ref i1 i))
                                (= (length (dict-ref i1 i)) (length (dict-ref i2 i)))
                                #t))
                          common)])
    (cons (sort-by-keyword (filter-common commonf i1 car))
          (sort-by-keyword (filter-common commonf i2 car)))))

                

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

; match-contour-order
; List of contours, List of contours -> List of contours
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

; match-glyphs-contours
; glyph, glyph -> glyph
; match the contours of the second glyph against the first

(define (match-glyphs-contours g1 g2)
  (let ([cs1 (glyph-contours g1)]
        [cs2 (glyph-contours g2)])
    (struct-copy glyph g2 [contours (match-contour-order cs1 cs2)])))

             
; match-fonts-contours
; font, font -> font
; match the contours of the second font against the first

(define (match-fonts-contours f1 f2)
  (let ([gs1 (font-glyphs f1)]
        [gs2 (font-glyphs f2)])
    (struct-copy font f2 [glyphs (map match-glyphs-contours gs1 gs2)])))
                  
             

(define (compatible-components c1 c2)
  (let ((common-comp (set->list 
                      (set-intersect (list->set (map car c1))
                                     (list->set (map car c2))))))
         (values (filter-common common-comp c1 car)
                 (filter-common common-comp c2 car))))


(define (compatible-anchors a1 a2)
  (let ((common-anc (set->list 
                     (set-intersect (list->set (map car a1))
                                    (list->set (map car a2))))))
         (values (filter-common common-anc a1 car)
                 (filter-common common-anc a2 car))))


    
           
    
(define (filter-common common lst key)
  (filter (lambda (i) (member (key i) common))
          lst))
                     
        
                                 

(define (component->matrix c)
  (match c
    [(list base x-scale xy-scale yx-scale y-scale x-offset y-offset) 
     (matrix 3 3 x-scale yx-scale 0 
             xy-scale y-scale 0
             x-offset y-offset 1)]))

(define (matrix->component m)
  (list (matrix-ref m 0 0) (matrix-ref m 0 1) (matrix-ref m 1 0) 
         (matrix-ref m 1 1) (matrix-ref m 0 2) (matrix-ref m 1 2)))
   
    
              
     
; decompose-glyph
; Font, Glyph, Symbol -> Glyph
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

(define (component->outlines c b)
  (define (transform-contour c m)
    (map (lambda (p)
           (vec->list (transform (list->vec p) m)))
         c))
  (let ([m (component->matrix c)]
        [base-contours (glyph-contours b)])
    (map (lambda (c) (transform-contour c m))
         base-contours)))