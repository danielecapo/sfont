#lang racket

(require "font.rkt"
         "glif.rkt"
         "vec.rkt"
         "fontpict.rkt"
         slideshow/pict-convert)

(provide (all-defined-out))

(struct flatfont (ufo info kerning glyphs) 
  #:transparent
  #:property prop:pict-convertible 
  (lambda (f)
    (let ([ascender (dict-ref (flatfont-info f) 'ascender 750)]
          [descender (dict-ref (flatfont-info f) 'descender -250)]
          [glyphs (map glyph->pict  (flatfont-glyphs f))])
      (apply pictf:font ascender descender glyphs))))

(define (ufo->flatfont f)
  (let ((f (ufo:sort-glyphs f)))
    (flatfont f
              (interpolable-infos (sort-by-keyword (hash->list (ufo:font-fontinfo f))))
              (sort-by-keyword 
               (hash-map (ufo:font-kerning f)
                         (lambda (left right)
                           (cons left (sort-by-keyword (hash->list right))))))
              (flat-glyphs f))))

(define (sort-by-keyword alist)
  (sort alist string<? 
        #:key (lambda (p) 
                (symbol->string (car p)))))

(define (flatfont->ufo f)
  (struct-copy ufo:font (flatfont-ufo f)
               [fontinfo (ufo-infos f)]
               [kerning (make-immutable-hash
                         (map (lambda (p) (cons (car p) (make-immutable-hash (cdr p))))
                              (flatfont-kerning f)))]
               [layers (list (struct-copy ufo:layer (ufo:get-layer (flatfont-ufo f))
                                          [glyphs (map (lambda (g) (flatglyph->ufo g (flatfont-ufo f)))  
                                                       (flatfont-glyphs f))]))]))


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
                 (flatfont-info f)
                 (lambda (key value)
                   (cons key ((car (dict-ref *infos* key)) value))))))
        (psname (dict-ref (ufo:font-fontinfo (flatfont-ufo f)) 'postscriptFontName "untitled"))
        (famname (dict-ref (ufo:font-fontinfo (flatfont-ufo f)) 'familyName "untitled")))
    (dict-set* infos 'postscriptFontName psname 'familyName famname)))

     
     
     
(define (pt x y) (list x y))

(struct flatglyph (name advance contours components anchors)
  #:transparent)

(define (glyph->pict g)
  
   (append (list (flatglyph-name g)
                 (car (flatglyph-advance g)))
           (map (lambda (c)
                  (letrec ((aux (lambda (pts acc)
                                 
                                  (match pts
                                    [(list-rest off1 off2 p rp)
                                     (aux rp (append acc (list (cons 'off off1) (cons 'off off2) p)))]
                                    [(list) acc]))))
                    (cons (cons 'move (car c))
                          (aux (cdr c) '()))))
                (flatglyph-contours g))))
  

(define (make-flatglyph g)
  (flatglyph (ufo:glyph-name g)
             (let ((a (ufo:glyph-advance g)))
               (pt (ufo:advance-width a) (ufo:advance-height a)))
             (ufo:map-contours flat-contour g)
             (ufo:map-components flat-component g)
             (ufo:map-anchors flat-anchor g)))

; flat-contour
; ufo:contour -> FlatContour
; produce a flat contour from a ufo:contour

(define (flat-contour c)
  (map vec->list (contour->bezier c)))

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
  (ufo:map-glyphs make-flatglyph f))

(define (flatglyph->ufo g ufo) 
  (match g
    [(flatglyph name (list aw ah) contours components anchors)
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
  (bezier->contour (map list->vec fc)))

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
  (struct-copy flatfont f
               [glyphs (filter (lambda (g)
                                 (member (flatglyph-name g) glyphs))
                                 (flatfont-glyphs f))]))


(define (sort-contours g [sort-points-fn identity])
  (struct-copy flatglyph g
               [contours 
                (sort (map sort-points-fn (flatglyph-contours g))
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

(define (sort-components g)
  (struct-copy flatglyph g
               [components (sort (flatglyph-components g)
                                 (lambda (c1 c2)
                                   (or (string<? (symbol->string (car c1))
                                                 (symbol->string (car c2)))
                                       (point<? (take-right c1 2) (take-right c2 2)))))]))

(define (sort-anchors g)
  (struct-copy flatglyph g
               [anchors (sort (flatglyph-anchors g)
                              #:key car
                              string<?)]))

                                                                                      
(define (prepare-for-interpolation f [weak #t])
  (let ((prep (lambda (g) (prepare-glyph g weak))))
    (struct-copy flatfont f
                 [glyphs (map prep (flatfont-glyphs f))])))

(define (prepare-glyph g [weak #t])
  (sort-components 
   (sort-anchors
    (sort-contours g (if weak identity canonical-start-point)))))

  
       
(define (compatible-fonts f1 f2)
  (let* ((common (set->list 
                  (set-intersect
                   (list->set (map flatglyph-name (flatfont-glyphs f1)))
                   (list->set (map flatglyph-name (flatfont-glyphs f2))))))
         (fc1 (keep-glyphs f1 common))
         (fc2 (keep-glyphs f2 common))
         (compatible-glyphs (filter
                             identity
                             (map compatible-glyphs 
                                 (flatfont-glyphs fc1)
                                 (flatfont-glyphs fc2))))
         (kerning (compatible-kerning
                   (flatfont-kerning f1) 
                   (flatfont-kerning f2)))
         (infos (compatible-infos
                 (flatfont-info f1) 
                 (flatfont-info f2))))
    (values (struct-copy flatfont fc1
                         [glyphs (map car compatible-glyphs)]
                         [kerning (car kerning)]
                         [info (car infos)])
            (struct-copy flatfont fc2
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
  (let [(common (set->list
                 (set-intersect (list->set (map car i1))
                                (list->set (map car i2)))))]
    (cons (sort-by-keyword (filter-common common i1 car))
          (sort-by-keyword (filter-common common i2 car)))))

                

(define (compatible-glyphs g1 g2)
  (let ((c1 (flatglyph-contours g1))
        (c2 (flatglyph-contours g2)))
    (if (and (= (length c1) (length c2))
             (andmap (lambda (a b) (= (length a) (length b)))
                     c1 c2))
        (let-values (((c1 c2) (compatible-components (flatglyph-components g1)
                                                     (flatglyph-components g2)))
                     ((a1 a2) (compatible-anchors (flatglyph-anchors g1)
                                                  (flatglyph-anchors g2))))
          (cons (struct-copy flatglyph g1 [components c1] [anchors a1])
                (struct-copy flatglyph g2 [components c2] [anchors a2])))
        #f)))

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
                     
        
                                 
   
    
              
     