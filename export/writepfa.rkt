#lang racket
(require "../geometry.rkt"
         "../utilities.rkt")

(provide 
 (contract-out 
  [type1/c (-> any/c boolean?)]
  [write-type1 (-> type1/c path-string? any)]
  [type1->string (-> type1/c string?)]))

;; write a better contract :)
(define type1/c (flat-named-contract 'type1/c list?))

;;;;  type1 example
#;
'(type1 (SourceSansPro-BlackIt 1.000)
       (fontdict
        (FontType 1)
        (FontName SourceSansPro-BlackIt)
        (FontInfo 
         (version "1.000")
         (Notice "Source is a trademark of Adobe Systems Incorporated in the United States and/or other countries.")
         (Copyright "Copyright 2010, 2012 Adobe Systems Incorporated. All Rights Reserved.")
         (FullName "Source Sans Pro Black Italic")
         (FamilyName "Source Sans Pro")
         (Weight "Black")
         (ItalicAngle -11)
         (isFixedPitch false)
         (UnderlinePosition -100)
         (UnderlineThickness 50))
        (PaintType 0)
        (FontMatrix [0.001 0 0 0.001 0 0])
        (Encoding StandardEncoding)
        (FontBBox (-216 -302 1190 978))
        %currentfile eexec dup 
        (Private
         (BlueValues [-12 0 500 512 532 544 580 592 634 646 650 662 696 708])
         (OtherBlues [-188 -176])
         (FamilyBlues [-12 0 486 498 518 530 574 586 638 650 656 668 712 724])
         (FamilyOtherBlues [-217 -205])
         (BlueScale 0.0625)
         (BlueFuzz 0)
         (StdHW [134])
         (StdVW [172])
         (StemSnapH [134])
         (StemSnapV [172 176]))))



; String Value -> String
(define (psdef name value)
  (format "~a ~a def" (->name name) value))

; Value -> String
(define (dict-begin l)
  (format "~a dict dup begin" l))

; Symbol -> String
(define (->name n)
  (string-append "/"(symbol->string n)))

; Integer -> String
(define (->int n)
  (if (exact-integer? n)
      (number->string n)
      (error "Required integer")))

; Real -> String
(define (->num n)
  (number->string n))

; (Listof Number) -> String
(define (->mat lst)
  (string-join (map ->num lst) " "
               #:before-first "["	 	 	 	 	 	 	 
               #:after-last "]"))

; (Listof Number) -> String
(define ->pvtarr ->mat)

; (Listof Number) -> String
(define (->bbox lst)
  (string-join (map ->num lst) " "
               #:before-first "{"	 	 	 	 	 	 	 
               #:after-last "}"))

; String -> String
(define (->string s)
  (format "(~a)" s))

; Boolean -> String
(define (->bool s)
 (if (boolean? s)
     (if s "true" "false")
     (error "Required boolean")))

; Symbol -> String
(define (->enc e)
 (cond [(and (symbol? e) (eq? e 'StandardEncoding))
                "StandardEncoding"]
               [(list? e) ""]
               [else (error "invalid encoding")]))


; Type1 -> String
(define (type1->string t)
  (match t
    [(list 'type1
           (list name version)
           (list-rest 'fontdict entries))
     (format "%!FontType1-1.1: ~a ~a
%%BeginResource: font ~a
~a
dup /FontName get exch definefont pop
mark
%currentfile closefile
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
cleartomark
%%EndResource
%%EOF"
             (symbol->string name)
             version
             (symbol->string name)
             (fontdict->string entries))]))

            
; T1FontDict -> String         
(define (fontdict->string entries)
  (define (aux e)
    (match e
      [(list-rest 'FontInfo is) (psdef 'Fontinfo (fontinfo->string is))]
      [`(FontName ,fn) (psdef 'FontName (->name fn))]
      [`(Encoding ,e) (psdef 'Encoding (->enc e))]
      [`(PaintType ,p) (psdef 'PaintType (->int p))]
      [`(FontType ,f) (psdef 'FontType (->int f))]
      [`(FontMatrix ,m) (psdef 'FontMatrix (->mat m))]
      [`(FontBBox ,b) (psdef 'FontBBox (->bbox b))]
      [`(UniqueID  ,u) (psdef 'UniqueID ->int)]
    ;(Metrics ->met)
      [`(StrokeWidth ,s) (psdef 'StrokeWidth (->num s))]
      [(list-rest 'Private ps) (private->string ps)] 
      [(list-rest 'CharStrings cs) (->charstrings cs)]))
  (string-append (dict-begin (length entries)) 
                 "\n" 
                 (string-join (map aux entries) "\n")
                 "\nend"))

; T1FontInfo -> String
(define (fontinfo->string entries)
  (define (aux e)
    (match e
      [`(version, v) (psdef 'version (->string v))]
      [`(Notice ,n) (psdef 'Notice (->string n))]
      [`(Copyright ,c) (psdef 'Copyright (->string c))]
      [`(FullName ,fn) (psdef 'FullName (->string fn))]
      [`(FamilyName ,fm) (psdef 'FamilyName (->string fm))]
      [`(Weight ,w) (psdef 'Weight (->string w))]
      [`(ItalicAngle ,i) (psdef 'ItalicAngle (->num i))]
      [`(isFixedPitch ,i) (psdef 'isFixedPitch (->bool i))]
      [`(UnderlinePosition ,u) (psdef 'UnderlinePosition (->num u))]
      [`(UnderlineThickness ,u) (psdef 'UnderlineThickness (->num u))]))
  (string-append (dict-begin (length entries)) "\n" 
                 (string-join (map aux entries) "\n")
                 "\nend"))

; T1PrivateDict -> String
(define (private->string entries)
  (define (aux e)
    (match e
      [`(BlueValues ,b) (psdef 'BlueValues (->pvtarr b))]
      [`(OtherBlues ,b) (psdef 'OtherBlues (->pvtarr b))]
      [`(FamilyBlues ,b) (psdef 'FamilyBlues (->pvtarr b))]
      [`(FamilyOtherBlues ,b) (psdef 'FamilyOtherBlues (->pvtarr b))]
      [`(BlueScale ,b) (psdef 'BlueScale (->num b))]
      [`(BlueFuzz ,b) (psdef 'BlueFuzz (->num b))]
      [`(StdHW ,s) (psdef 'StdHW (->pvtarr s))]
      [`(StdVW ,s) (psdef 'StdVW (->pvtarr s))]
      [`(StemSnapH ,s) (psdef 'StemSnapH (->pvtarr s))]
      [`(StemSnapV ,s) (psdef 'StemSnapV (->pvtarr s))]
      [`(ExpansionFactor ,e) (psdef 'ExpansionFactor (->num e))]
      [`(ForceBold ,fb) (psdef 'ForceBold (->bool fb))]
      [`(LanguageGroup ,l) (psdef 'LanguageGroup (->int l))]))
  (string-append "end\n"
                 "%currentfile eexec dup /Private\n"
                 (dict-begin (+ (length entries) 7))
                 "\n"
                 "/-| {string currentfile exch readstring pop} def"
                 "\n"
                 "/|- {def} def"
                 "\n"
                 "/| {put} def"
                 "\n"
                 (string-join (map aux entries) "\n")
                 "\n"
                 "/password 5839 def"
                 "\n"
                 "/MinFeature {16 16} def"
                 "\n"
                 "/OtherSubrs[{}{}{}{systemdict/internaldict known not{pop 3}{1183615869"
                 "\n"
                 "systemdict/internaldict get exec dup/startlock known{/startlock get exec}{dup"
                 "\n"
                 "/strtlck known{/strtlck get exec}{pop 3}ifelse}ifelse}ifelse}executeonly]def"
                 "\n"
                 "/Subrs 5 array"
                 "\n"
                 "dup 0 ## -| { 3 0 callother pop pop setcurrentpoint return } |"
                 "\n"
                 "dup 1 ## -| { 0 1 callother return } |"
                 "\n"
                 "dup 2 ## -| { 0 2 callother return } |"
                 "\n"
                 "dup 3 ## -| { return } |"
                 "\n"
                 "dup 4 ## -| { 3 1 3 callother pop callsubr return } |"
                 "\n"
                 "def"
                 "\n"
                 "put"))

; T1CharStrings -> String
(define (->charstrings cs)
  (format "dup /CharStrings\n~a\n~a\nend put"
          (dict-begin (length cs))
          (string-join (map char->string cs) "\n")))


;;;;;;;;;;;;;;;
; At the moment this function doesn't replace the last line segment
; of a closed path with a closepath, but it use a zero-length closepath.
; see Type1 Spec 4.3 Conciseness

; T1CharString -> String
;;; big warning
;;; I don't round coordinates

(define (char->string c)
  (define (seg->ps segment)
    (match segment
      [(list (vec x y) (vec x y) (vec x2 y2) (vec x2 y2))
       (format "~a ~a rlineto" (- x2 x) (- y2 y))]
      [(list (vec x y) (vec xc1 yc1) (vec xc2 yc2) (vec x2 y2))
       (format "~a ~a ~a ~a ~a ~a rrcurveto" 
               (- xc1 x) (- yc1 y) (- xc2 xc1)
               (- yc2 yc1) (- x2 xc2) (- y2 yc2))]))
  
  (define (segs->ps segments s)
    (match segments
      [(list (list (vec x y) (vec x y) (vec x2 y2) (vec x2 y2)))
       (cons (string-append s "\n" "closepath") (vec x y))]
      [(list (list (vec x y) (vec xc1 yc1) (vec xc2 yc2) (vec x2 y2)))
       (cons (string-append s "\n" 
                            (format "~a ~a ~a ~a ~a ~a rrcurveto closepath" 
                                    (- xc1 x) (- yc1 y) (- xc2 xc1)
                                    (- yc2 yc1) (- x2 xc2) (- y2 yc2)))
             (vec x2 y2))]
      [(list-rest (list (vec x y) (vec x y) (vec x2 y2) (vec x2 y2)) r)
       (segs->ps r (string-append s "\n" (format "~a ~a rlineto" (- x2 x) (- y2 y))))]
      [(list-rest (list (vec x y) (vec xc1 yc1) (vec xc2 yc2) (vec x2 y2)) r)
       (segs->ps r (string-append s "\n" 
                                 (format "~a ~a ~a ~a ~a ~a rrcurveto" 
                                         (- xc1 x) (- yc1 y) (- xc2 xc1)
                                         (- yc2 yc1) (- x2 xc2) (- y2 yc2))))]
      ))
  (define (bez->ps b zero)
    (let ([move (vec- (car b) zero)])
      (segs->ps (segments b 3)
                (format "~a ~a rmoveto" (vec-x move) (vec-y move)))))
                
  (string-join 
   (list (->name (car c))
         "## -| {"
         (if (= (cdadr c) 0)
             (format "0 ~a hsbw" (num->int (caadr c)))
             (format "0 0 ~a ~a sbw" 
                    (num->int (caadr c))
                    (num->int (cdadr c))))
         (cdr 
          (foldl (lambda (b acc)
                   (let ([r (bez->ps b (car acc))])
                     (cons (cdr r)
                         (string-append (cdr acc) "\n" (car r)))))
                 (cons (vec 0 0) "")
                 (cddr c)))
         "endchar } |-")
   " "))
  

; Type1 Path -> Any
(define (write-type1 f path)
  (with-output-to-file path
    (lambda () (printf (type1->string f)))))
                        
      