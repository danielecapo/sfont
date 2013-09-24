#lang racket

(require "draw-property.rkt"
         "../fontpict.rkt"
         racket/gui/base
         "../math.rkt"
         "../ufo.rkt"
         "../utilities.rkt")

#;
(define-fonts 
  (light bold) 
  (read-ufo "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_0.ufo")
  (read-ufo "/Users/daniele/Downloads/source-sans-pro-master/RomanMM/SourceSansPro_1.ufo"))

(define frame 
  (new frame%
       [label "Example"]
       [width 1000]
       [height 500]))

(define (make-canvas par proc)
  (new canvas% [parent par]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-initial-matrix (vector 1 0 0 1 0 0))
          (send dc set-smoothing 'smoothed)
          (proc dc 1.2 *text* *size*))]))

(struct world (current-state) #:mutable)

(define (animate update-proc start end inc-proc)
    (let* ([w (world (update-proc start))]
           [area-height (* *size* (+ 1 (lines *text*) (* (- 1.2 1) (lines *text*))))]
           [frame (new frame%
                      [label "Viewer"]
                      [width 1000]
                      [height (num->int area-height)])]
           [canv (new canvas% 
                   [parent frame]
                   [paint-callback
                    (lambda (canvas dc)
                      (send dc set-initial-matrix (vector 1 0 0 1 0 0))
                      (send dc set-smoothing 'smoothed)
                      ((get-drawing-proc (world-current-state w)) dc 1.2 *text* *size*))])])
      (letrec ([aux (lambda (c)
                      (if (> c end)
                          (world-current-state w)
                          (begin
                            (set-world-current-state! w (update-proc c))
                            (send canv refresh-now)
                            (aux (inc-proc c)))))])
        (begin 
          (send frame show #t)
          (aux (inc-proc start))))))

(struct slider-editor
  (text size inputs)
  #:transparent)

(define (update-inputs e f v)
  (struct-copy slider-editor e
               [inputs (hash-set (slider-editor-inputs e) f v)]))

(define (update-text e t)
  (struct-copy slider-editor e
               [text t]))

(define (update-size e s)
  (struct-copy slider-editor e
               [size s]))

(define (get-input e f)
  (hash-ref (slider-editor-inputs e) f))

(define-syntax slider-application
  (syntax-rules (font sliders update)
    [(_ [font font-proc get-ufo] 
        [sliders (sl-name sl-min sl-max) ...])
     (lambda (state)
       (let* ([w (world state)]
              [updater (lambda (f v can)
                         (begin
                           (set-world-current-state! 
                            w 
                            (update-inputs (world-current-state w) f v))
                           ;(print (world-current-state w))
                           (send can refresh-now)))]
              [text-updater (lambda (t can)
                             (begin
                               (set-world-current-state! 
                                w (update-text (world-current-state w) t))
                               (send can refresh-now)))]
              [size-updater (lambda (s can)
                             (begin
                               (set-world-current-state! 
                                w (update-size (world-current-state w) (string->number s)))
                               (send can refresh-now)))]
                                                         
              [frame (new frame%
                          [label "sfont"]
                          [width 1000]
                          [height 400])]
              [gen (new vertical-pane%
                        [parent frame])]
              [tf (new horizontal-pane%
                        [parent gen]
                        [stretchable-height #f])]
            
              [pan (new horizontal-pane%
                        [parent gen])]
              [sidebar (new vertical-panel%
                            [parent pan]
                            [stretchable-width #f]
                            [alignment '(right bottom)])]
                            
              [sls (new vertical-pane%
                        [parent sidebar]
                        [min-width 300]
                        [stretchable-width #f])]
              [can (new canvas%
                        [parent pan]
                        [min-width 700]
                        [min-height 350]
                        [stretchable-width #t]
                        [stretchable-height #t]
                        [paint-callback
                         (lambda (canvas dc)
                           (send dc set-initial-matrix (vector 1 0 0 1 0 0))
                           (send dc set-smoothing 'smoothed)
                           (let* ([e (world-current-state w)]
                                  [is (map ((curry get-input) e) '(sl-name ...))])
                             ((get-drawing-proc (apply font-proc is)) dc 1.2 (string->text (slider-editor-text e)) (slider-editor-size e))))]
                        )]
              [t (new text-field%
                      [label "Text"]
                      [parent tf]
                      [init-value (slider-editor-text (world-current-state w))]
                      [callback (lambda (tf e) (text-updater (send tf get-value) can))]
                      [min-width 800])]
              [size (new combo-field%	 
                         [label "Size"]	 
                         [choices (map number->string (range 10 500 10))]	 
                         [parent tf]
                         [init-value (number->string (slider-editor-size (world-current-state w)))]
                         [callback (lambda (cf e) (size-updater (send cf get-value) can))]
                         [stretchable-width #f])]
              [generate (new button%
                             [label "Generate"]
                             [parent sidebar]
                             [callback (lambda (b e)
                                         (let ([filepath (put-file "Generate UFO" frame #f "untitled.ufo" "ufo")])
                                           (when filepath
                                             (write-ufo 
                                              (get-ufo
                                               (apply font-proc 
                                                      (map ((curry get-input) (world-current-state w)) '(sl-name ...))))
                                              filepath))))])]
              )
                         
         (let ([sl-name (new slider%
                             [parent sls]
                             [style '(horizontal vertical-label)]
                             [label (symbol->string 'sl-name)]
                             [min-value sl-min]
                             [max-value sl-max]
                             [init-value (get-input (world-current-state w) 'sl-name)]
                             [callback (lambda (s e) (updater 'sl-name (send s get-value) can))])]
               ...)
             (send frame show #t))))]))
     
                    
(require "../examples/fontwrite-square.rkt")

(define (sq-proc weight width)
    (sq #:weight (/ weight 1000) #:width (/ width 1000)))
      
((slider-application
   [font sq-proc identity]
   [sliders
    (weight 0 1000)
    (width 0 1000)])
   (slider-editor "abc" 120 (hash 'weight 500 'width 500)))

;(make-canvas frame (get-drawing-proc light))