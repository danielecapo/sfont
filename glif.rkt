#lang racket
(require xml
         xml/path
         "plists.rkt"
         "bezier.rkt"
         "vec.rkt"
         "properties.rkt"
         "fontpict.rkt"
          (planet wmfarr/plt-linalg:1:13/matrix)
          slideshow/pict-convert)


(provide read-glif-file
         write-glif-file
         (struct-out glyph)
         (struct-out advance)
         (struct-out image)
         (struct-out guideline)
         (struct-out anchor)
         (struct-out contour)
         (struct-out component)
         (struct-out point)
         glyph-round
         advance-round
         image-round
         guideline-round
         anchor-round
         contour-round
         component-round
         point-round
         make-advance
         make-guideline
         make-image
         make-anchor
         make-contour
         make-component
         make-point
         glyph1->glyph2
         glyph2->glyph1
         map-contours
         for-each-contours
         map-components
         for-each-components
         map-anchors
         for-each-anchors
         map-guidelines
         for-each-guidelines
         map-points
         for-each-points
         draw-glyph
         contour->bezier
         bezier->contour
         component->outlines
         contour-open?
         reverse-contour
         glyph-reverse-directions
         glyph-correct-directions)
         
         
         
         
(struct glyph (format name advance unicodes note image
                         guidelines anchors contours components lib) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (glyph-transform v m))
  #:property prop:pict-convertible 
  (lambda (g)
    (let* ([cs (map-contours contour->bezier g)]
           [bb (if (null? cs)
                   (cons (vec 0 0) (vec 0 0))
                   (apply combine-bounding-boxes
                          (map bezier-bounding-box cs)))])
      (pictf:glyph (draw-glyph g) bb))))
                      

; glyph-transform
; glyph, TransformationMatrix -> glyph
; produces a new glyph by applying the transformation matrix to contours, component and anchors 

(define (glyph-transform g m)
  (let ([tfn (lambda (o) (transform o m))])
    (struct-copy glyph g
                 [anchors    (map tfn (glyph-anchors g))]
                 [components (map tfn (glyph-components g))]
                 [contours   (map tfn (glyph-contours g))])))

; glyph-round
; Glyph -> Glyph
; Round the coordinates of the glyph using the current *precision* factor

(define (glyph-round g)
  (struct-copy glyph g
               [advance (advance-round (glyph-advance g))]
               [image (if (glyph-image g)
                          (image-round (glyph-image g))
                          #f)]
               [guidelines (map guideline-round (glyph-guidelines g))]
               [anchors (map anchor-round (glyph-anchors g))]
               [contours (map contour-round (glyph-contours g))]
               [components (map component-round (glyph-components g))]))

(struct advance (width height) #:transparent)

; advance-round
; Advance -> Advance
; Round the coordinates of the advance using the current *precision* factor

(define (advance-round a)
  (struct-copy advance a 
               [width (approx (advance-width a))]
               [height (approx (advance-height a))]))

(struct image (filename x-scale xy-scale yx-scale 
                            y-scale x-offset y-offset color) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (image-transform v m)))

; image-transform
; image, TransformationMatrix -> image
; produces a new image applying the transformation matrix to image

(define (image-transform i m)
  (apply image
         (append 
          (cons (image-filename i)
                (matrix->ufo-matrix 
                 (composed-transformation-matrix (image->matrix i) m)))
          (list (image-color i)))))

; image->matrix 
; image -> TransformationMatrix
; produces a transformation matrix from the image

(define (image->matrix i)
  (match i 
    [(image _ x-scale xy-scale yx-scale y-scale x-offset y-offset _)
     (list->matrix '((,x-scale ,xy-scale ,x-offset)
                     (,yx-scale ,y-scale ,y-offset)
                     (0 0 1)))]))
         
; image-round
; Image -> Image
; Round the coordinates of the image using the current *precision* factor

(define (image-round i)
  (struct-copy image i 
               [x-offset (approx (image-x-offset i))]
               [y-offset (approx (image-y-offset i))]))


(struct guideline (x y angle name color identifier) 
  #:transparent)

; guideline-round
; Guideline -> Guideline
; Round the coordinates of the guideline using the current *precision* factor

(define (guideline-round g)
  (struct-copy guideline g 
               [x (approx (guideline-x g))]
               [y (approx (guideline-y g))]))

(struct anchor (x y name color identifier) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (anchor-transform v m)))

; anchor-transform
; anchor, TransformationMatrix -> anchor
; produces a new anchor applying the transformation matrix to anchor

(define (anchor-transform a m)
  (let ([v (transform (vec (anchor-x a) (anchor-y a)) m)])
    (struct-copy anchor a
                 [x (vec-x v)] [y (vec-y v)])))

; anchor-round
; Anchor -> Anchor
; Round the coordinates of the anchor using the current *precision* factor

(define (anchor-round a)
  (struct-copy anchor a 
               [x (approx (anchor-x a))]
               [y (approx (anchor-y a))]))

(struct contour (identifier points) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (contour-transform v m)))

; contour-transform
; contour, TransformationMatrix -> contour
; produces a new contour applying the transformation matrix to contour

(define (contour-transform c m)
  (struct-copy contour c
               [points (map (lambda (p) (transform p m))
                            (contour-points c))]))

; contour-round
; Contour -> Contour
; Round the coordinates of the contour using the current *precision* factor

(define (contour-round c)
  (struct-copy contour c 
               [points (map point-round (contour-points c))]))


(struct component (base x-scale xy-scale yx-scale y-scale 
                            x-offset y-offset identifier) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (component-transform v m)))

; component-transform
; component, TransformationMatrix -> component
; produces a new component applying the transformation matrix to component

(define (component-transform c m)
  (apply component
         (append 
          (cons (component-base c)
                (matrix->ufo-matrix 
                 (composed-transformation-matrix (component->matrix c) m)))
          (list (component-identifier c)))))


; component-round
; Component -> Component
; Round the coordinates of the component using the current *precision* factor

(define (component-round c)
  (struct-copy component c 
               [x-offset (approx (component-x-offset c))]
               [y-offset (approx (component-y-offset c))]))

; component->matrix 
; component -> TransformationMatrix
; produce a transformation matrix from the component

(define (component->matrix i)
  (match i 
    [(component _ x-scale xy-scale yx-scale y-scale x-offset y-offset _)
     (list->matrix `((,x-scale ,xy-scale ,x-offset)
                     (,yx-scale ,y-scale ,y-offset)
                     (0 0 1)))]))

(struct point (x y type smooth name identifier) 
  #:transparent
  #:property prop:transform 
  (lambda (v m) (point-transform v m)))

; point-transform
; point, TransformationMatrix -> point
; produce a new point applying the transformation matrix to point

(define (point-transform p m)
  (let ([v (transform (vec (point-x p) (point-y p)) m)])
    (struct-copy point p
                 [x (vec-x v)] [y (vec-y v)])))

; point-round
; Point -> Point
; Round the coordinates of the point using the current *precision* factor

(define (point-round p)
  (struct-copy point p 
               [x (approx (point-x p))]
               [y (approx (point-y p))]))

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




(define (make-advance #:width [width 0] #:height [height 0])
  (advance (ensure-number width) (ensure-number height)))

(define (make-image #:fileName [filename #f] #:xScale [x-scale 1] #:xyScale [xy-scale 0] 
                        #:yxScale [yx-scale 0] #:yScale [y-scale 0] #:xOffset [x-offset 0]
                        #:yOffset [y-offset 0] #:color [color #f])
  (image filename (ensure-number x-scale) (ensure-number xy-scale) 
             (ensure-number yx-scale) (ensure-number y-scale) 
             (ensure-number x-offset) (ensure-number y-offset) (ensure-color color)))


(define (make-guideline #:x [x #f] #:y [y #f]  #:angle [angle #f] 
                            #:name [name #f] #:color [color #f] 
                            #:identifier [identifier #f])
  (guideline (ensure-number x) (ensure-number y) (ensure-number angle) name (ensure-color color) (ensure-symbol identifier)))

(define (make-anchor #:x [x #f] #:y [y #f] #:name [name #f] 
                         #:color [color #f] #:identifier [identifier #f])
  (anchor (ensure-number x) (ensure-number y) name (ensure-color color) (ensure-symbol identifier)))

(define (make-contour #:identifier [identifier #f] #:points [points null])
  (contour (ensure-symbol identifier) points))

(define (make-component #:base [base #f]  #:xScale [x-scale 1] #:xyScale [xy-scale 0] 
                        #:yxScale [yx-scale 0] #:yScale [y-scale 1] #:xOffset [x-offset 0]
                        #:yOffset [y-offset 0] #:identifier [identifier #f])
  (component (ensure-symbol base) (ensure-number x-scale) (ensure-number xy-scale) 
                 (ensure-number yx-scale) (ensure-number y-scale) 
                 (ensure-number x-offset) (ensure-number y-offset) (ensure-symbol identifier)))

(define (make-point #:x [x #f] #:y [y #f] #:type [type 'offcurve] 
                        #:smooth [smooth #f] #:name [name #f] #:identifier [identifier #f])
  (point (ensure-number x) (ensure-number y) (ensure-symbol type)
             (ensure-smooth smooth) name (ensure-symbol identifier)))


(define (map-contours proc glyph)
  (map proc (glyph-contours glyph)))

(define (for-each-contours proc glyph)
  (for-each proc (glyph-contours glyph)))

(define (map-components proc glyph)
  (map proc (glyph-components glyph)))

(define (for-each-components proc glyph)
  (for-each proc (glyph-components glyph)))

(define (map-guidelines proc glyph)
  (map proc (glyph-guidelines glyph)))

(define (for-each-guidelines proc glyph)
  (for-each proc (glyph-guidelines glyph)))

(define (map-anchors proc glyph)
  (map proc (glyph-anchors glyph)))

(define (for-each-anchors proc glyph)
  (for-each proc (glyph-anchors glyph)))

(define (map-points proc contour)
  (map proc (contour-points contour)))

(define (for-each-points proc contour)
  (for-each proc (contour-points contour)))


(define (draw-glyph g)
  (append (list (glyph-name g)
                (advance-width (glyph-advance g)))
          (map-contours contour->bezier g)))

(define (draw-contour c)
  (letrec ((aux (lambda (pts)
                  (match pts
                    [(list-rest (point _ _ 'offcurve _ _ _) rest-points)
                     (aux (append rest-points (list (car pts))))]
                     [_ pts]))))
    (draw-points (aux (contour-points c)))))

(define (draw-points pts)
  (let* ((first-pt (car pts))
         (rest-pts (cdr pts))
         (start (list (point-x first-pt) 
                      (point-y first-pt))))                      
    (cons (cons 'move start)
          (append (map (lambda (pt)
                         (match pt 
                           [(point x y 'offcurve _ _ _)
                            `(off ,x ,y)]
                           [(point x y _ _ _ _)
                            `(,x ,y)]))
                       rest-pts)
                  (list start)))))
                          
   


(define (glyph2->glyph1 g)
  (match g
    [(glyph format name advance (list codes ...) note image 
                guidelines anchors contours components lib)
     (glyph 1 name advance codes #f #f null null 
                (append 
                 (map (lambda (c) 
                       (match c
                         [(contour _ points)
                          (contour 
                           #f (map (lambda (p) 
                                     (struct-copy point p [identifier #f]))
                                   points))]))
                       contours)
                 (map anchor->contour anchors))
                (map (lambda (c) 
                       (struct-copy component c [identifier #f]))
                       components)
                lib)]))

(define (anchor->contour a)
  (make-contour #:points (list (make-point #:x (anchor-x a)
                                             #:y (anchor-y a)
                                             #:name (anchor-name a)
                                             #:type 'move))))

(define (glyph1->glyph2 g)
  (struct-copy glyph g [format 2]))

                
                
  
  
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
  (apply-with-kws make-point (cadr x)))

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
                       (list (make-anchor #:x x #:y y #:name name)))
               elts)]
         [(list-rest 'contour id points)
          (aux (append contours
                       (list (apply-with-kws 
                              make-contour 
                              (append id (list (list 'points (map parse-point points)))))))
               components
               anchors
               elts)]
         [(list 'component args)
          (aux contours
               (append components (list (apply-with-kws
                                         make-component
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
          (aux (struct-copy glyph acc 
                            [advance (apply-with-kws make-advance args)])
               restelts)]
         [(list 'unicode (list (list 'hex hex)))
          (aux (struct-copy glyph acc 
                            [unicodes (append (glyph-unicodes acc) (list (string->unicode hex)))])
               restelts)]
         [(list 'note null n)
          (aux (struct-copy glyph acc [note n])
               restelts)]
         [(list 'image args)
          (aux (struct-copy glyph acc 
                            [image (apply-with-kws make-image args)])
               restelts)]
         [(list 'guideline args)
          (aux (struct-copy glyph acc 
                            [guidelines (cons (apply-with-kws make-guideline args)
                                              (glyph-guidelines acc))])
               restelts)]
         [(list 'anchor args)
          (aux (struct-copy glyph acc 
                            [anchors (cons (apply-with-kws make-anchor args)
                                              (glyph-anchors acc))])
               restelts)]
         [(list-rest 'outline null outlines)
          (let-values ([(contours components anchors) (parse-outlines outlines)])
            (aux (struct-copy glyph
                              (struct-copy glyph acc [contours contours])
                              [components components]
                              [anchors (append (glyph-anchors acc) anchors)])
                 restelts))]
         [(list 'lib null d)
          (aux (struct-copy glyph acc 
                            [lib (xexpr->dict d)])
               restelts)]
         
         [_ acc])]))
  (aux (glyph (string->number (se-path* '(glyph #:format) x))
                  (if name name (string->symbol (se-path* '(glyph #:name) x)))
                  (make-advance) null #f #f null null null null #f)
       (se-path*/list '(glyph) x)))

(define-syntax-rule (not-default val defaultvalue expr)
    (if (equal? val defaultvalue) '() (list expr)))

(define (glyph->xexpr g)
  (letrec [(aux 
            (lambda (g)
              (match g
                [#f '()]
                [(glyph format name advance codes note image 
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
                [(advance width height)
                 `(advance (,@(list `(width ,(number->string width)))
                            ,@(not-default height 0 `(width ,(number->string height)))))]
                [(image filename xs xys yxs ys xo yo color)
                 `((image ((fileName ,filename)
                           ,@(not-default xs 1 `(xScale ,(number->string xs)))
                           ,@(not-default xys 0 `(xyScale ,(number->string xys)))
                           ,@(not-default yxs 0 `(yxScale ,(number->string yxs)))
                           ,@(not-default ys 1 `(yScale ,(number->string ys)))
                           ,@(not-default xo 0 `(xOffset ,(number->string xo)))
                           ,@(not-default yo 0 `(yOffset ,(number->string yo)))
                           ,@(not-default color #f `(color ,(color->string color))))))]
                
                [(guideline x y angle name color identifier)
                 `(guideline (,@(not-default x #f `(x ,(number->string x)))
                              ,@(not-default y #f `(y ,(number->string y)))
                              ,@(not-default angle #f `(angle ,(number->string angle)))
                              ,@(not-default name #f `(name ,name))
                              ,@(not-default color #f `(color ,(color->string color)))
                              ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(anchor x y name color identifier)
                 `(anchor (,@(not-default x #f `(x ,(number->string x)))
                           ,@(not-default y #f `(y ,(number->string y)))
                           ,@(not-default name #f `(name ,name))
                           ,@(not-default color #f `(color ,(color->string color)))
                           ,@(not-default identifier #f `(identifier ,(symbol->string identifier)))))]
                [(contour id points)
                 `(contour (,@(not-default id #f `(identifier ,(symbol->string id))))
                           ,@(map (lambda (p) (aux p)) points))]
                [(point x y type smooth name id)
                 `(point ((x ,(number->string x)) 
                          (y ,(number->string y))
                          ,@(not-default type 'offcurve `(type ,(symbol->string type)))
                          ,@(not-default smooth #f `(smooth "yes"))
                          ,@(not-default name #f `(name ,name))
                          ,@(not-default id #f `(identifier ,(symbol->string id)))))]
                
                [(component base xs xys yxs ys xo yo id)
                 `(component ((base ,(symbol->string base))
                              ,@(not-default xs 1 `(xScale ,(number->string xs)))
                              ,@(not-default xys 0 `(xyScale ,(number->string xys)))
                              ,@(not-default yxs 0 `(yxScale ,(number->string yxs)))
                              ,@(not-default ys 1 `(yScale ,(number->string ys)))
                              ,@(not-default xo 0 `(xOffset ,(number->string xo)))
                              ,@(not-default yo 0 `(yOffset ,(number->string yo)))
                              ,@(not-default id #f `(identifier ,(symbol->string id)))))]
      
      )))]
    (aux (if (= (glyph-format g) 1)
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
; contour -> Bezier
; Transform a contour in a bezier curve (i.e. all segments are made by 4 points)

(define (contour->bezier c)
  (letrec ((ensure-first-on-curve 
            (lambda (pts)
              (match pts
                [(list-rest (point _ _ 'move _ _ _) pr) pts]
                [(list-rest (point _ _ 'curve _ _ _) pr) pts]
                [(list-rest (point _ _ 'line _ _ _) pr) pts]
                [(list-rest (point _ _ 'qcurve _ _ _) pr) pts]
                [(list-rest (point _ _ 'offcurve _ _ _) pr) 
                 (ensure-first-on-curve (append pr (list (car pts))))])))
           (flattener 
            (lambda (pts acc)
              (match pts
                [(list-rest (or
                             (point x y 'curve _ _ _)
                             (point x y 'move _ _ _)
                             (point x y 'line _ _ _))
                            (point x1 y1 'line _ _ _)
                            _)
                 (flattener (cdr pts) (append acc (list (vec x y) (vec x y)(vec x1 y1))))]
                [(list-rest (point x y 'offcurve _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list-rest (point x y 'curve _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list-rest (point x y 'move _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list-rest (point x y 'line _ _ _) pr)
                 (flattener pr (append acc (list (vec x y))))]
                [(list) acc]))))
    (let* ((points (ensure-first-on-curve (contour-points c)))
           (first-point (car points)))
      (if (eq? (point-type first-point) 'move)
          (flattener points '())
          (flattener (append points (list first-point)) '())))))

; bezier -> contour
; Bezier -> contour
; Transform a bezier curve in a contour 


(define (bezier->contour b)
  (letrec ((aux 
            (lambda (prev pts acc)
              (match (cons prev pts)
                [(list-rest (vec x y) (vec x y) (vec x2 y2) (vec x2 y2) rest-pts)
                   (aux (vec x2 y2) rest-pts (append acc (list (make-point #:x x2 #:y y2 #:type 'line))))]
                [(list-rest (vec x y) (vec ox1 oy1) (vec ox2 oy2) (vec x2 y2) rest-pts)
                 (aux (vec x2 y2) rest-pts (append acc
                                                  (list (make-point #:x ox1 #:y oy1)
                                                        (make-point #:x ox2 #:y oy2)
                                                        (make-point #:x x2 #:y y2 #:type 'curve))))]
                [(list _) acc]
                [(list) null]))))
    (let* ((first-pt (car b))
           (ufo-pts (aux first-pt (cdr b) null)))
      (make-contour #:points 
                        (if (closed? b) ufo-pts
                            (cons (make-point #:x (vec-x first-pt)
                                              #:y (vec-y first-pt)
                                              #:type 'move)
                                  ufo-pts))))))
   

; component->outlines
; component, glyph -> List of contour
; produce a list of contours from a component applying the trasformation matrix to the contours in the base

(define (component->outlines c b)
  (let ([m (component->matrix c)]
        [base-contours (glyph-contours b)])
    (map (lambda (c) (transform c m))
         base-contours)))
     


; matrix->ufo-matrix
; TransformationMatrix -> List of Numbers
; Produces a represetantion of the transf. mat. useful for ufo objects
(define (matrix->ufo-matrix m)
  (map approx
       (list (matrix-ref m 0 0) (matrix-ref m 0 1) (matrix-ref m 1 0) 
             (matrix-ref m 1 1) (matrix-ref m 0 2) (matrix-ref m 1 2))))


; contour-open?
; contour -> Boolean
; True if the contour starts with a point of type 'move

(define (contour-open? c)
  (eq? 'move (point-type (car (contour-points c)))))

; reverse-contour
; contour -> contour
; returns the contour with reversed point list

(define (reverse-contour c)
  (if (contour-open? c)
      c
      (struct-copy contour c
                   [points (contour-points 
                            (bezier->contour 
                             (reverse (contour->bezier c))))])))

; glyph-reverse-directions
; glyph -> glyph
; reverse the direction of all contours in the glyph

(define (glyph-reverse-directions g)
  (struct-copy glyph g 
               [contours (map reverse-contour 
                              (glyph-contours g))]))

; glyph-correct-directions
; glyph -> glyph
; reverse the direction of all contours in the glyph if the area is negative

(define (glyph-correct-directions g)
  (let* ([cs (map contour->bezier (glyph-contours g))]
         [a (foldl (lambda (b acc) 
                     (+ acc (bezier-signed-area b)))
                   0 cs)])
    (if (< a 0)
        (struct-copy glyph g 
                     [contours (map (lambda (c b)
                                      (struct-copy contour c
                                                   [points (contour-points (bezier->contour (reverse b)))]))
                                    (glyph-contours g) cs)])
        g)))

; 
;(define-syntax-rule (~ elts ...)
;  (let ((first-elt (car elts)))
;    (letrec (aux (lambda (elts acc)
;                   (match elts
;                     [(list-rest `(,x ,y) `(,x1 y1) rest-elts)
;                      (aux rest-elts
;                           (append acc
;                                   (list (make-point #:x x #:y #:type 'curve)
;                                         (make-point #:x x #:y #:type 'line))))]
;                     [(list-rest `(,x ,y) `(,x1 y1 c) rest-elts)
;                      (aux (cdr elts) (append acc (list (make-point #:x x #:y #:type 'curve))))]
;                     [(list-rest `(,x y c) rest-elts)
;                      (aux rest-elts (append acc (list (make-point #:x x #:y y))))]
;                     [(list `(,x ,y) 'close) 
;                      (append acc (
;                                    

;(define g (read-glif-file "/Users/daniele/glif.glif"))

;(define g1 (xexpr->glyph g))