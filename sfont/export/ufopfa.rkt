#lang racket
(require "../main.rkt"
         "../geometry.rkt"
         "writepfa.rkt"
         "../utilities.rkt")

(provide 
 (contract-out 
  [ufo->type1 (-> font? type1/c)]))

(define (get-or-default k d)
  (lambda (i)
    (hash-ref i k d)))

(define INFODEFAULT `((version ,(lambda (i) 
                                 (let ([im (hash-ref i 'versionMinor #f)]
                                       [iM (hash-ref i 'versionMajor #f)])
                                   (if (and im iM)
                                       (string-append (number->string iM) "." (number->string im))
                                       "1.000"))))
                      (Notice ,(get-or-default 'copyright ""))
                      (Copyright ,(get-or-default 'copyright ""))
                      (FullName ,(lambda (i)
                                   (hash-ref i 'postscriptFullName
                                             (~a (hash-ref i 'familyName "Untitled") " "
                                                 (hash-ref i 'postscriptWeightName "Regular")))))                           
                      (FamilyName ,(get-or-default 'familyName "Untitled"))
                      (Weight ,(get-or-default 'postscriptWeightName "Regular"))
                      (ItalicAngle ,(get-or-default 'italicAngle 0))
                      (isFixedPitch ,(get-or-default 'postscriptIsFixedPitch #f))
                      (UnderlinePosition ,(get-or-default 'postscriptUnderlinePosition -100))
                      (UnderlineThickness ,(get-or-default 'postscriptUnderlineThickness 50))))


(define PRIVATEDEAFULT `((BlueValues ,(get-or-default 'postscriptBlueValues null))
                         (OtherBlues ,(get-or-default 'postscriptOtherBlues null))
                         (FamilyBlues ,(get-or-default 'postscriptFamilyBlues null))
                         (FamilyOtherBlues ,(get-or-default 'postscriptFamilyOtherBlues null))
                         (BlueScale ,(get-or-default 'postscriptBlueScale 0.039625))
                         (BlueFuzz ,(get-or-default 'postscriptBlueFuzz 0))
                         (StdHW ,(lambda (p)
                                   (let ([snap (hash-ref p 'postscriptStemSnapH #f)])
                                     (if (and snap (and (> (length snap) 0)))
                                         (list (car snap)) null))))
                         (StdVW ,(lambda (p)
                                   (let ([snap (hash-ref p 'postscriptStemSnapV #f)])
                                     (if (and snap (and (> (length snap) 0)))
                                         (list (car snap)) null))))
                         (StemSnapH ,(get-or-default 'postscriptStemSnapH null))
                         (StemSnapV ,(get-or-default 'postscriptStemSnapV null))
                         (ForceBold ,(get-or-default 'postscriptForceBold null))))

(define (convert h)
  (lambda (d)
    (list (car d) ((cadr d) h))))


; Info -> T1FontInfo
(define (ufoinfo->t1fontinfo info)
  (cons 'FontInfo 
        (filter (lambda (o) (not (equal? "" (cadr o))))
                (map (convert info) INFODEFAULT))))


; Info -> T1PrivateDict
(define (ufoprivate->t1privatedict info)
  (cons 'Private
        (filter (lambda (o) (not (null? (cadr o))))
                (map (convert info) PRIVATEDEAFULT))))


; Glyph -> T1CharString
; produce a glyph ready to be written in type1 format
; warning: remove open paths
(define (ufoglyph->t1charstring g)
  (cons
   (glyph-name g)
   (cons
    (cons (advance-width (glyph-advance g))
          (advance-height (glyph-advance g)))
    (filter (lambda (b) (closed? b)) 
            (map-contours 
             (compose contour->bezier contour-round) g)))))

; Font -> Type1
(define (ufo->type1 f [fbbox #f])
  (let* ([l (decompose-layer f foreground)]
         [charstrings (map-glyphs ufoglyph->t1charstring l)]
         [gbs (filter (lambda (b) (not (null? b))) (map cddr charstrings))]
         [info (font-fontinfo f)]
         [fname (string->symbol ((get-or-default 'postscriptFontName "Untitled") info))]
         [s (/ 1.0 ((get-or-default 'unitsPerEm 1000) info))]
         [mat (list s 0 0 s 0 0)]
         [fontinfo (ufoinfo->t1fontinfo info)]
         [pvt (ufoprivate->t1privatedict info)]
         [version (car (dict-ref (cdr fontinfo) 'version))]
         [bbox (if fbbox fbbox 
                   (apply combine-bounding-boxes 
                          (map bezier-bounding-box (apply append gbs))))])
    `(type1 (,fname ,version)
            (fontdict
             (FontName ,fname)
             (FontType 1)
             (PaintType 0)
             (Encoding StandardEncoding)
             ,fontinfo
             (FontMatrix ,mat)
             (FontBBox ,(list (num->int (vec-x (car bbox))) (num->int (vec-y (car bbox)))
                              (num->int (vec-x (cdr bbox))) (num->int (vec-y (cdr bbox)))))
             ,pvt
             ,(cons 'CharStrings charstrings)))))


              