#lang racket

(require "../ufo.rkt"
         "../geometry.rkt"
         "interpolables.rkt")


(provide 
 (contract-out 
  [info-scale (->* (fontinfo/c real?) (real?) fontinfo/c)]
  [kerning-scale (-> kerning/c real? kerning/c)]))

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

; FontInfo Real Real -> FontInfo
(define (info-scale i fx [fy fx])
  (make-immutable-hash
   (hash-map i (lambda (key value)
                 (cons key 
                      (((car (dict-ref *info-transform* key)) 
                        * value) fx fy))))))

; Kerning Real -> Kerning
(define (kerning-scale k f)
  (map-kerning ((curry *) f) k))
                    