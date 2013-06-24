#lang racket
(require xml
         xml/path
         "plists.rkt"
         "bezier.rkt"
         "vec.rkt"
         "properties.rkt"
          (planet wmfarr/plt-linalg:1:13/matrix))


(provide read-glif-file
         write-glif-file
         (struct-out ufo:glyph)
         (struct-out ufo:advance)
         (struct-out ufo:image)
         (struct-out ufo:guideline)
         (struct-out ufo:anchor)
         (struct-out ufo:contour)
         (struct-out ufo:component)
         (struct-out ufo:point)
         ufo:make-advance
         ufo:make-guideline
         ufo:make-image
         ufo:make-anchor
         ufo:make-contour
         ufo:make-component
         ufo:make-point
         glyph1->glyph2
         glyph2->glyph1
         ufo:map-contours
         ufo:for-each-contours
         ufo:map-components
         ufo:for-each-components
         ufo:map-anchors
         ufo:for-each-anchors
         ufo:map-guidelines
         ufo:for-each-guidelines
         ufo:map-points
         ufo:for-each-points
         draw-glyph
         contour->bezier
         bezier->contour
         ufo:component->outlines)
         
         
         
         
(struct ufo:glyph (format name advance unicodes note image
                         guidelines anchors contours components lib) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (glyph-transform v m)))

; glyph-transform
; ufo:glyph, TransformationMatrix -> ufo:glyph
; produces a new glyph by applying the transformation matrix to contours, component and anchors 

(define (glyph-transform g m)
  (let ([tfn (lambda (o) (transform o m))])
    (struct-copy ufo:glyph g
                 [anchors    (map tfn (ufo:glyph-anchors g))]
                 [components (map tfn (ufo:glyph-components g))]
                 [contours   (map tfn (ufo:glyph-contours g))])))

(struct ufo:advance (width height) #:transparent)

(struct ufo:image (filename x-scale xy-scale yx-scale 
                            y-scale x-offset y-offset color) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (image-transform v m)))

; image-transform
; ufo:image, TransformationMatrix -> ufo:image
; produces a new image applying the transformation matrix to image

(define (image-transform i m)
  (apply ufo:image
         (append 
          (cons (ufo:image-filename i)
                (matrix->ufo-matrix 
                 (composed-transformation-matrix (image->matrix i) m)))
          (list (ufo:image-color i)))))

; image->matrix 
; ufo:image -> TransformationMatrix
; produces a transformation matrix from the image

(define (image->matrix i)
  (match i 
    [(ufo:image _ x-scale xy-scale yx-scale y-scale x-offset y-offset _)
     (list->matrix '((,x-scale ,xy-scale ,x-offset)
                     (,yx-scale ,y-scale ,y-offset)
                     (0 0 1)))]))
         

(struct ufo:guideline (x y angle name color identifier) 
  #:transparent)


(struct ufo:anchor (x y name color identifier) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (anchor-transform v m)))

; anchor-transform
; ufo:anchor, TransformationMatrix -> ufo:anchor
; produces a new anchor applying the transformation matrix to anchor

(define (anchor-transform a m)
  (let ([v (transform (vec (ufo:anchor-x a) (ufo:anchor-y a)) m)])
    (struct-copy ufo:anchor a
                 [x (vec-x v)] [y (vec-y v)])))

(struct ufo:contour (identifier points) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (contour-transform v m)))

; contour-transform
; ufo:contour, TransformationMatrix -> ufo:contour
; produces a new contour applying the transformation matrix to contour

(define (contour-transform c m)
  (struct-copy ufo:contour c
               [points (map (lambda (p) (transform p m))
                            (ufo:contour-points c))]))


(struct ufo:component (base x-scale xy-scale yx-scale y-scale 
                            x-offset y-offset identifier) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (component-transform v m)))

; component-transform
; ufo:component, TransformationMatrix -> ufo:component
; produces a new component applying the transformation matrix to component

(define (component-transform c m)
  (apply ufo:component
         (append 
          (cons (ufo:component-base c)
                (matrix->ufo-matrix 
                 (composed-transformation-matrix (component->matrix c) m)))
          (list (ufo:component-identifier c)))))

; component->matrix 
; ufo:component -> TransformationMatrix
; produces a transformation matrix from the component

(define (component->matrix i)
  (match i 
    [(ufo:component _ x-scale xy-scale yx-scale y-scale x-offset y-offset _)
     (list->matrix `((,x-scale ,xy-scale ,x-offset)
                     (,yx-scale ,y-scale ,y-offset)
                     (0 0 1)))]))

(struct ufo:point (x y type smooth name identifier) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (point-transform v m)))

; point-transform
; ufo:point, TransformationMatrix -> ufo:point
; produces a new point applying the transformation matrix to point

(define (point-transform p m)
  (let ([v (transform (vec (ufo:point-x p) (ufo:point-y p)) m)])
    (struct-copy ufo:point p
                 [x (vec-x v)] [y (vec-y v)])))

(define (ensure-number n)
  (if (or (not n) (number? n)) n (string->number n)))

(define (ensure-symbol s)
  (if s
      (if (symbol? s) s (string->symbol s))
      s))

(define (ensure-smooth s)
  (match s
    [#f #f]
    ["no" #f]
    [#t #t]
    ["yes" #t]))


(define (string->color s)
  (map (lambda (s) (string->number (string-trim s))) 
       (string-split s ",")))

(define (color->string c)
  (string-join (map number->string c) ","))

(define (ensure-color c)
  (if (string? c) (string->color c) c))

(define (string->unicode s)
  (string->number (string-append "#x" s)))

(define (unicode->string n)
  (~r n #:base '(up 16) #:pad-string "0" #:min-width 4))




(define (ufo:make-advance #:width [width 0] #:height [height 0])
  (ufo:advance (ensure-number width) (ensure-number height)))

(define (ufo:make-image #:fileName [filename #f] #:xScale [x-scale 1] #:xyScale [xy-scale 0] 
                        #:yxScale [yx-scale 0] #:yScale [y-scale 0] #:xOffset [x-offset 0]
                        #:yOffset [y-offset 0] #:color [color #f])
  (ufo:image filename (ensure-number x-scale) (ensure-number xy-scale) 
             (ensure-number yx-scale) (ensure-number y-scale) 
             (ensure-number x-offset) (ensure-number y-offset) (ensure-color color)))


(define (ufo:make-guideline #:x [x #f] #:y [y #f]  #:angle [angle #f] 
                            #:name [name #f] #:color [color #f] 
                            #:identifier [identifier #f])
  (ufo:guideline (ensure-number x) (ensure-number y) (ensure-number angle) name (ensure-color color) (ensure-symbol identifier)))

(define (ufo:make-anchor #:x [x #f] #:y [y #f] #:name [name #f] 
                         #:color [color #f] #:identifier [identifier #f])
  (ufo:anchor (ensure-number x) (ensure-number y) name (ensure-color color) (ensure-symbol identifier)))

(define (ufo:make-contour #:identifier [identifier #f] #:points [points null])
  (ufo:contour (ensure-symbol identifier) points))

(define (ufo:make-component #:base [base #f]  #:xScale [x-scale 1] #:xyScale [xy-scale 0] 
                        #:yxScale [yx-scale 0] #:yScale [y-scale 1] #:xOffset [x-offset 0]
                        #:yOffset [y-offset 0] #:identifier [identifier #f])
  (ufo:component (ensure-symbol base) (ensure-number x-scale) (ensure-number xy-scale) 
                 (ensure-number yx-scale) (ensure-number y-scale) 
                 (ensure-number x-offset) (ensure-number y-offset) (ensure-symbol identifier)))

(define (ufo:make-point #:x [x #f] #:y [y #f] #:type [type 'offcurve] 
                        #:smooth [smooth #f] #:name [name #f] #:identifier [identifier #f])
  (ufo:point (ensure-number x) (ensure-number y) (ensure-symbol type)
             (ensure-smooth smooth) name (ensure-symbol identifier)))


(define (ufo:map-contours proc glyph)
  (map proc (ufo:glyph-contours glyph)))

(define (ufo:for-each-contours proc glyph)
  (for-each proc (ufo:glyph-contours glyph)))

(define (ufo:map-components proc glyph)
  (map proc (ufo:glyph-components glyph)))

(define (ufo:for-each-components proc glyph)
  (for-each proc (ufo:glyph-components glyph)))

(define (ufo:map-guidelines proc glyph)
  (map proc (ufo:glyph-guidelines glyph)))

(define (ufo:for-each-guidelines proc glyph)
  (for-each proc (ufo:glyph-guidelines glyph)))

(define (ufo:map-anchors proc glyph)
  (map proc (ufo:glyph-anchors glyph)))

(define (ufo:for-each-anchors proc glyph)
  (for-each proc (ufo:glyph-anchors glyph)))

(define (ufo:map-points proc contour)
  (map proc (ufo:contour-points contour)))

(define (ufo:for-each-points proc contour)
  (for-each proc (ufo:contour-points contour)))


(define (draw-glyph g)
  (append (list (ufo:glyph-name g)
                (ufo:advance-width (ufo:glyph-advance g)))
          (ufo:map-contours draw-contour g)))

(define (draw-contour c)
  (letrec ((aux (lambda (pts)
                  (match pts
                    [(list-rest (ufo:point _ _ 'offcurve _ _ _) rest-points)
                     (aux (append rest-points (list (car pts))))]
                     [_ pts]))))
    (draw-points (aux (ufo:contour-points c)))))

(define (draw-points pts)
  (let* ((first-pt (car pts))
         (rest-pts (cdr pts))
         (start (list (ufo:point-x first-pt) 
                      (ufo:point-y first-pt))))                      
    (cons (cons 'move start)
          (append (map (lambda (pt)
                         (match pt 
                           [(ufo:point x y 'offcurve _ _ _)
                            `(off ,x ,y)]
                           [(ufo:point x y _ _ _ _)
                            `(,x ,y)]))
                       rest-pts)
                  (list start)))))
                          
   


(define (glyph2->glyph1 g)
  (match g
    [(ufo:glyph format name advance (list codes ...) note image 
                guidelines anchors contours components lib)
     (ufo:glyph 1 name advance codes #f #f null null 
                (append 
                 (map (lambda (c) 
                       (match c
                         [(ufo:contour _ points)
                          (ufo:contour 
                           #f (map (lambda (p) 
                                     (struct-copy ufo:point p [identifier #f]))
                                   points))]))
                       contours)
                 (map anchor->contour anchors))
                (map (lambda (c) 
                       (struct-copy ufo:component c [identifier #f]))
                       components)
                lib)]))

(define (anchor->contour a)
  (ufo:make-contour #:points (list (ufo:make-point #:x (ufo:anchor-x a)
                                             #:y (ufo:anchor-y a)
                                             #:name (ufo:anchor-name a)
                                             #:type 'move))))

(define (glyph1->glyph2 g)
  (struct-copy ufo:glyph g [format 2]))

                
                
  
  
(define (collect-keywords kvs)
  (map (lambda (kv) 
         (string->keyword 
          (symbol->string (car kv)))) 
       kvs))

(define (collect-values kvs)
  (map cadr kvs))

(define (apply-with-kws proc kvs)
  (keyword-apply proc (collect-keywords kvs) (collect-values kvs) '()))
                 
(define (parse-point x)
  (apply-with-kws ufo:make-point (cadr x)))

(define (parse-outlines os)
  (define (aux contours components anchors elts)
    (match elts
      [(list) (values contours components anchors)]
      [(list-rest e elts)
       (match e
         [(list 'contour id (list point (list-no-order '(type "move") `(name ,name) `(x ,x) `(y ,y))))
          (aux contours 
               components 
               (append anchors 
                       (list (ufo:make-anchor #:x x #:y y #:name name)))
               elts)]
         [(list-rest 'contour id points)
          (aux (append contours
                       (list (apply-with-kws 
                              ufo:make-contour 
                              (append id (list (list 'points (map parse-point points)))))))
               components
               anchors
               elts)]
         [(list 'component args)
          (aux contours
               (append components (list (apply-with-kws
                                         ufo:make-component
                                         args)))
               anchors
               elts)])]))
         
  (aux null null null os))

(define (xexpr->glyph x [name #f])
  (define (aux acc elts)
    (match elts
      [(list) acc]
      [(list-rest elt restelts)
       (match elt
         
         [(list 'advance args) 
          (aux (struct-copy ufo:glyph acc 
                            [advance (apply-with-kws ufo:make-advance args)])
               restelts)]
         [(list 'unicode (list (list 'hex hex)))
          (aux (struct-copy ufo:glyph acc 
                            [unicodes (append (ufo:glyph-unicodes acc) (list (string->unicode hex)))])
               restelts)]
         [(list 'note null n)
          (aux (struct-copy ufo:glyph acc [note n])
               restelts)]
         [(list 'image args)
          (aux (struct-copy ufo:glyph acc 
                            [image (apply-with-kws ufo:make-image args)])
               restelts)]
         [(list 'guideline args)
          (aux (struct-copy ufo:glyph acc 
                            [guidelines (cons (apply-with-kws ufo:make-guideline args)
                                              (ufo:glyph-guidelines acc))])
               restelts)]
         [(list 'anchor args)
          (aux (struct-copy ufo:glyph acc 
                            [anchors (cons (apply-with-kws ufo:make-anchor args)
                                              (ufo:glyph-anchors acc))])
               restelts)]
         [(list-rest 'outline null outlines)
          (let-values ([(contours components anchors) (parse-outlines outlines)])
            (aux (struct-copy ufo:glyph
                              (struct-copy ufo:glyph acc [contours contours])
                              [components components]
                              [anchors (append (ufo:glyph-anchors acc) anchors)])
                 restelts))]
         [(list 'lib null d)
          (aux (struct-copy ufo:glyph acc 
                            [lib (xexpr->dict d)])
               restelts)]
         
         [_ acc])]))
  (aux (ufo:glyph (string->number (se-path* '(glyph #:format) x))
                  (if name name (string->symbol (se-path* '(glyph #:name) x)))
                  (ufo:make-advance) null #f #f null null null null #f)
       (se-path*/list '(glyph) x)))

(define-syntax-rule (not-default val defaultvalue expr)
    (if (equal? val defaultvalue) '() (list expr)))

(define (glyph->xexpr g)
  (letrec [(aux 
            (lambda (g)
              (match g
                [#f '()]
                [(ufo:glyph format name advance codes note image 
                            guidelines anchors contours components lib)
                 `(glyph ((format ,(number->string format))
                          (name ,(symbol->string name)))
                         ,(aux advance)
                         ,@(map (lambda (c) `(unicode ((hex ,(unicode->string c))))) codes)
                         ,@(not-default note #f `(note () ,note))
                         ;(if note `((note () ,note)) '())
                         ,@(aux image)
                         ,@(map (lambda (guideline) (aux guideline)) guidelines)
                         ,@(map (lambda (anchor) (aux anchor)) anchors)
                         (outline ,@(map (lambda (contour) (aux contour)) contours)
                                  ,@(map (lambda (component) (aux component)) components))
                         (lib ,@(not-default lib #f (dict->xexpr lib)))
                         )]
                [(ufo:advance width height)
                 `(advance (,@(not-default width 0 `(width ,(number->string width)))
                            ,@(not-default height 0 `(width ,(number->string height)))))]
                [(ufo:image filename xs xys yxs ys xo yo color)
                 `((image ((fileName ,filename)
                           ,@(not-default xs 1 `(xScale ,(number->string xs)))
                           ,@(not-default xys 0 `(xyScale ,(number->string xys)))
                           ,@(not-default yxs 0 `(yxScale ,(number->string yxs)))
                           ,@(not-default ys 1 `(yScale ,(number->string ys)))
                           ,@(not-default xo 0 `(xOffset ,(number->string xo)))
                           ,@(not-default yo 0 `(yOffset ,(number->string yo)))
                           ,@(not-default color #f `(color ,(color->string color))))))]
                
                [(ufo:guideline x y angle name color identifier)
                 `(guideline (,@(not-default x #f `(x ,(number->string x)))
                              ,@(not-default y #f `(y ,(number->string y)))
                              ,@(not-default angle #f `(angle ,(number->string angle)))
                              ,@(not-default name #f `(name ,name))
                              ,@(not-default color #f `(color ,(color->string color)))
                              ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(ufo:anchor x y name color identifier)
                 `(anchor (,@(not-default x #f `(x ,(number->string x)))
                           ,@(not-default y #f `(y ,(number->string y)))
                           ,@(not-default name #f `(name ,name))
                           ,@(not-default color #f `(color ,(color->string color)))
                           ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(ufo:contour id points)
                 `(contour (,@(not-default id #f `(identifier ,(symbol->string id))))
                           ,@(map (lambda (p) (aux p)) points))]
                [(ufo:point x y type smooth name id)
                 `(point ((x ,(number->string x)) 
                          (y ,(number->string y))
                          ,@(not-default type 'offcurve `(type ,(symbol->string type)))
                          ,@(not-default smooth #f `(smooth "yes"))
                          ,@(not-default name #f `(name ,name))
                          ,@(not-default id #f `(identifier ,(symbol->string id)))))]
                
                [(ufo:component base xs xys yxs ys xo yo id)
                 `(component ((base ,(symbol->string base))
                              ,@(not-default xs 1 `(xScale ,(number->string xs)))
                              ,@(not-default xys 0 `(xyScale ,(number->string xys)))
                              ,@(not-default yxs 0 `(yxScale ,(number->string yxs)))
                              ,@(not-default ys 1 `(yScale ,(number->string ys)))
                              ,@(not-default xo 0 `(xOffset ,(number->string xo)))
                              ,@(not-default yo 0 `(yOffset ,(number->string yo)))
                              ,@(not-default id #f `(identifier ,(symbol->string id)))))]
      
      )))]
    (aux (if (= (ufo:glyph-format g) 1)
             (glyph2->glyph1 g)
             g))))
             
     
(define (read-glif-file path [name #f])
  (xexpr->glyph
   (xml->xexpr 
   ((eliminate-whitespace 
     '(glyph advance unicode image guideline anchor 
             outline contour point component lib dict array)
     identity)
   (document-element
    (call-with-input-file path read-xml))))
   name))

(define (write-glif-file g path)
  (call-with-output-file
      path
    (lambda (o)
      (parameterize ([empty-tag-shorthand 'always])
                   (write-xml 
                    (document
                     (prolog (list (p-i (location 1 0 1) 
                                        (location 1 38 39) 
                                        'xml "version=\"1.0\" encoding=\"UTF-8\""))
                             #f '())
                     (xexpr->xml (glyph->xexpr g))
                     '())
                    o)))
    #:exists 'replace))
       

; contour->bezier
; ufo:contour -> Bezier
; Transform a ufo:contour in a bezier curve (i.e. all segments are made by 4 points)

(define (contour->bezier c)
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
                 (flattener (cdr pts) (append acc (list (vec x y) (vec x y)(vec x1 y1))))]
                [(list-rest (ufo:point x y 'offcurve _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list-rest (ufo:point x y 'curve _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list-rest (ufo:point x y 'move _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list-rest (ufo:point x y 'line _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list) acc]))))
    (let* ((points (ensure-first-on-curve (ufo:contour-points c)))
           (first-point (car points)))
      (if (eq? (ufo:point-type first-point) 'move)
          (flattener points '())
          (flattener (append points (list first-point)) '())))))

; bezier -> contour
; Bezier -> ufo:contour
; Transform a bezier curve in a ufo:contour 


(define (bezier->contour b)
  (letrec ((aux 
            (lambda (prev pts acc)
              (match (cons prev pts)
                [(list-rest (vec x y) (vec x y) (vec x2 y2) (vec x2 y2) rest-pts)
                   (aux (vec x2 y2) rest-pts (append acc (list (ufo:make-point #:x x2 #:y y2 #:type 'line))))]
                [(list-rest (vec x y) (vec ox1 oy1) (vec ox2 oy2) (vec x2 y2) rest-pts)
                 (aux (vec x2 y2) rest-pts (append acc
                                                  (list (ufo:make-point #:x ox1 #:y oy1)
                                                        (ufo:make-point #:x ox2 #:y oy2)
                                                        (ufo:make-point #:x x2 #:y y2 #:type 'curve))))]
                [(list _) acc]
                [(list) null]))))
    (let* ((first-pt (car b))
           (ufo-pts (aux first-pt (cdr b) null)))
      (ufo:make-contour #:points 
                        (if (closed? b) ufo-pts
                            (cons (ufo:make-point #:x (car first-pt)
                                                  #:y (cadr first-pt)
                                                  #:type 'move)
                                  ufo-pts))))))
   

; ufo:component->outlines
; ufo:component, glyph -> List of ufo:contour
; produce a list of contours from a component applying the trasformation matrix to the contours in the base

(define (ufo:component->outlines c b)
  (let ([m (component->matrix c)]
        [base-contours (ufo:glyph-contours b)])
    (map (lambda (c) (transform c m))
         base-contours)))
     


; matrix->ufo-matrix
; TransformationMatrix -> List of Numbers
; Produces a represetantion of the transf. mat. useful for ufo objects
(define (matrix->ufo-matrix m)
  (map approx
       (list (matrix-ref m 0 0) (matrix-ref m 0 1) (matrix-ref m 1 0) 
             (matrix-ref m 1 1) (matrix-ref m 0 2) (matrix-ref m 1 2))))

; 
;(define-syntax-rule (~ elts ...)
;  (let ((first-elt (car elts)))
;    (letrec (aux (lambda (elts acc)
;                   (match elts
;                     [(list-rest `(,x ,y) `(,x1 y1) rest-elts)
;                      (aux rest-elts
;                           (append acc
;                                   (list (ufo:make-point #:x x #:y #:type 'curve)
;                                         (ufo:make-point #:x x #:y #:type 'line))))]
;                     [(list-rest `(,x ,y) `(,x1 y1 c) rest-elts)
;                      (aux (cdr elts) (append acc (list (ufo:make-point #:x x #:y #:type 'curve))))]
;                     [(list-rest `(,x y c) rest-elts)
;                      (aux rest-elts (append acc (list (ufo:make-point #:x x #:y y))))]
;                     [(list `(,x ,y) 'close) 
;                      (append acc (
;                                    

;(define g (read-glif-file "/Users/daniele/glif.glif"))

;(define g1 (xexpr->glyph g))