#lang racket

(require (planet wmfarr/plt-linalg:1:13/matrix)
         "flatfont.rkt"
         "vec.rkt")
(provide (all-defined-out))

                      
(define (font:transform o m)
  ((match o
     [(font _ _ _ _) font-transform]
     [(glyph _ _ _ _ _) glyph-transform]
     [(list (list a b) ...) contour-transform]
     [(list _ _ _) anchor-transform]
     [(list _ _ _ _ _ _ _) component-transform])
   o m))

(define (font:scale o sx [sy sx])
  ((match o
     [(font _ _ _ _) font-scale]
     [(glyph _ _ _ _ _) glyph-scale]
     [(list (list a b) ...) contour-scale]
     [(list _ _ _) anchor-scale]
     [(list _ _ _ _ _ _ _) component-scale])
   o sx sy))
     

(define-syntax-rule (define-transform (id args ...) matrix-form)
  (define (id o args ...)
    (font:transform o matrix-form)))

(define-transform (font:translate v)
  (translation-matrix (vec-x v) (vec-y v)))

(define-transform (font:rotate angle)
  (rotation-matrix angle))

(define-transform (font:skew-x angle)
  (shear-matrix (- (approx (tan angle))) 0))

(define-transform (font:skew-y angle)
  (shear-matrix 0 (approx (tan angle))))

(define-transform (font:reflect-x)
  (scale-matrix -1 1))

(define-transform (font:reflect-y)
  (scale-matrix 1 -1))



;[advance 
;                (vec->list 
;                 (transform 
;                  (list->vec (glyph-advance g))
;                  m))]
  
(define (font-transform f m)
  (struct-copy font f
               [glyphs (map (lambda (g) (glyph-transform g m))
                            (font-glyphs f))]))

(define (font-scale f sx [sy sx])
  (struct-copy font (glyphs-scale f sx sy)
               [info (info-scale (font-info f) sx sy)]))

;; glyphs-scale
;; Font, Number, Number -> Font
;; produce a new font with glyphs and kerning scaled (do not affect info)
(define (glyphs-scale f sx [sy sx])
  (struct-copy font f
               [info (font-info f)]
               [kerning (kerning-scale (font-kerning f) sx)]
               [glyphs (map (lambda (g) (glyph-scale g sx sy))
                            (font-glyphs f))]))



(define (kerning-scale kern s)
  (map (lambda (k)
         (cons (car k) 
               (map (lambda (r)
                      (cons (car r) (* (cdr r) s)))
                    (cdr k))))
       kern))

(define (info-scale i sx [sy sx])
  (dict-map i (lambda (key value)
                (cons key 
                      (((car (dict-ref *info-transform* key)) 
                        * value) sx sy)))))

    
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
               
(define (glyph-transform g m)
  (struct-copy glyph g
               [contours 
                (map (lambda (c) (contour-transform c m))
                     (glyph-contours g))]
               [components 
                (map (lambda (c) (component-transform c m))
                     (glyph-components g))]
               [anchors 
                (map (lambda (a) (anchor-transform a m))
                     (glyph-anchors g))]))

(define (glyph-scale g sx [sy sx])
  (struct-copy glyph g
               [contours 
                (map (lambda (c) (contour-scale c sx sy))
                     (glyph-contours g))]
               [components 
                (map (lambda (c) (component-scale c sx sy))
                     (glyph-components g))]
               [anchors 
                (map (lambda (a) (anchor-scale a sx sy))
                     (glyph-anchors g))]
               [advance (vec->list 
                         (vec-quick-scale (list->vec (glyph-advance g))
                                          sx sy))]))

(define (contour-transform c m)
  (map (lambda (p)
         (vec->list (transform (list->vec p) m)))
       c))

(define (contour-scale c sx [sy sx])
  (map (lambda (p)
         (vec->list (vec-quick-scale (list->vec p) sx sy)))
       c))

(define (anchor-transform a m)
  (match a
    [(list name x y) (cons name (vec->list (transform (vec x y) m)))]))

(define (anchor-scale a sx [sy sx])
  (match a
    [(list name x y) (cons name (vec->list (vec-quick-scale (vec x y) sx sy)))]))




(define (component-transform c m)
  (match c
    [(list base x-scale xy-scale yx-scale y-scale x-offset y-offset) 
     (cons base (matrix->component (matrix-mul m (component->matrix c))))]))

(define (component-scale c sx [sy sx])
  (match c
    [(list base x-scale xy-scale yx-scale y-scale x-offset y-offset) 
     (list base (* x-scale sx) (* xy-scale sx) (* yx-scale sy) (* y-scale sy) (* x-offset sx) (* y-offset sy))])) 

(define (contour+ c1 . cs)
  (apply map (lambda (p1 . ps)
               (vec->list (foldl vec+ (vec 0 0) 
                                 (map list->vec (cons p1 ps)))))
         c1 cs))

(define (component+ c1 . cs)
  (cons (car c1)
        (foldl (lambda (cc1 cc2) (map + cc1 cc2))
               (cdr c1)
               (map cdr cs))))

(define (anchor+ a1 . as)
  (cons (car a1)
        (vec->list (foldl vec+ (list->vec (cdr a1))
                          (map (lambda (a) (list->vec (cdr a)))
                               as)))))

(define (glyph+ g1 . gs)
  (let [(gss (cons g1 gs))]
    (struct-copy glyph g1
                 [advance 
                  (vec->list 
                   (foldl vec+ (vec 0 0)
                          (map (lambda (g) 
                                 (list->vec 
                                  (glyph-advance g)))
                               gss)))]
                 [contours
                  (apply map contour+ (map glyph-contours gss))]
                 [components
                  (apply map component+ (map glyph-components gss))]
                 [anchors
                  (apply map anchor+ (map glyph-anchors gss))])))


(define (font+ f1 . fs)
  (let [(fonts (cons f1 fs))]
  (struct-copy font f1
               [info (apply info+ (map font-info fonts))]
               [kerning (apply kerning+ (map font-kerning fonts))]
               [glyphs (apply map glyph+ (map font-glyphs fonts))])))

(define (info+ i1 . is)
  (letrec [(aux (lambda (i1 i2)
                  (dict-map i1
                            (lambda (key value)
                              (let [(v2 (dict-ref i2 key))]
                                (cons key
                                      (match value
                                        [(list _ ...) (map + value v2)]
                                        [(? number? value) (+ value v2)]
                                        [_ (error "cannot add info")])))))))]
    (foldl aux i1 is)))

(define (kerning+ k1 . ks)
  (letrec [(aux (lambda (k1 k2)
                  (map (lambda (kl1 kl2)
                         (cons (car kl1)
                               (map (lambda (kr1 kr2)
                                      (cons (car kr1)
                                            (+ (cdr kr1) (cdr kr2))))
                                    (cdr kl1) (cdr kl2))))
                       k1 k2)))]
    (foldl aux k1 ks)))

(define (font:* o s1 . ss)
  (font:scale o (apply * (cons s1 ss))))

(define (font:+ o1 . os)
  (apply (match o1
           [(font _ _ _ _) font+]
           [(glyph _ _ _ _ _) glyph+]
           [(list (list a b) ...) contour+]
           [(list _ _ _) anchor+]
           [(list _ _ _ _ _ _ _) component+])
         (cons o1 os)))

(define (font:- o1 . os)
  (apply font:+ (cons o1 (map (lambda (o) (font:* o -1))
                                  os))))

(define (font:/ o s1 . ss)
  (font:* o (apply * (map (lambda (s) 
                                (/ 1 s)) 
                              (cons s1 ss)))))


(define (x-> o)
  (font:scale o 1 0))

(define (y-> o)
  (font:scale o 0 1))  