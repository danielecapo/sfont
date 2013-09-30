#lang racket

(require "../main.rkt")


(provide 
 (contract-out 
  [info-scale (->* (fontinfo/c real?) (real?) fontinfo/c)]
  [kerning-scale (-> kerning/c real? kerning/c)]
  [info+ (->* (fontinfo/c) () #:rest (listof fontinfo/c) fontinfo/c)]
  [kerning+ (->* (kerning/c) () #:rest (listof kerning/c) kerning/c)]))


; FontInfo Real Real -> FontInfo
(define (info-scale i fx [fy fx])
  (make-immutable-hash
   (hash-map i (lambda (key value)
                 (let ([r (dict-ref *info-transform* key #f)])
                   (if r 
                       (cons key 
                             (((car r) * value) fx fy))
                       (cons key value)))))))

; FontInfo ... -> FontInfo
(define (info+ i1 . is)
  (letrec [(aux (lambda (i1 i2)
                  (make-immutable-hash
                   (hash-map i1
                             (lambda (key value)
                               (let [(v2 (hash-ref i2 key))]
                                 (cons key
                                       (match value
                                         [(list _ ...) (map + value v2)]
                                         [(? real? value) (+ value v2)]
                                         [_  value]))))))))]
        (foldl aux i1 is)))


; Kerning Real -> Kerning
(define (kerning-scale k f)
  (map-kerning ((curry *) f) k))
                    
; Kerning ... -> Kerning
(define (kerning+ k1 . ks)
  (letrec ([right-kerns
            (lambda (r1 r2)
              (make-immutable-hash
               (hash-map r1 (lambda (r v)
                              (cons r (+ v (hash-ref r2 r)))))))]
           [aux (lambda (k1 k2)
                  (make-immutable-hash
                   (hash-map k1 (lambda (l rk)
                                  (cons l (right-kerns rk (hash-ref k2 l)))))))])
    (foldl aux k1 ks)))

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
