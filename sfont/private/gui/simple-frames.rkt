#lang racket

(require "draw-property.rkt"
         racket/gui/base
         "../../main.rkt"
         "../../utilities.rkt")

(provide 
 (contract-out 
  (animate (->* ((-> real? font?) natural-number/c natural-number/c) (real? real? (-> real? real?)) font?)))
 animate-fonts
 slider-application)
  


(struct world (current-state) #:mutable)

; (Real -> Font) Real Real (Real -> Real) -> Font
; Show an animation frame and return the last font in animation
; The idea is that the user call animate with a function that accept a time parameter
; and return a font at time t, start and end numbers, and an increment function.
; Default value mean that time start at 0, ends at 100 with increments of 10

; I understand this is not a good way to do this, probably one can use the slideshow language 
(define (animate font-proc  width height [start 0] [end 100] [inc-proc (curry + 10)])
    (let* ([w (world (make-bitmap width height))]
           [frame (new frame%
                      [label "Viewer"]
                      [width width]
                      [height height])]
           [canv (new canvas% 
                   [parent frame]
                   [paint-callback
                    (lambda (canvas dc)
                      (send dc draw-bitmap (world-current-state w) 0 0))])])
    (letrec ([aux (lambda (i)
                    (if (> i end) '()
                        (let* ([img (make-bitmap width height)]
                               [dc (new bitmap-dc% [bitmap img])])
                          (begin
                            (send dc set-smoothing 'smoothed)
                            ((get-drawing-proc (font-proc i)) dc 1.2 (display-text) (display-size)) 
                            (cons img (aux (inc-proc i)))))))])
      (let ([bms (aux start)])
        (begin
          (send frame show #t)
          (for-each (lambda (bm)
                      (begin 
                        (set-world-current-state! w bm)
                        (sleep 0.1)
                        (send canv refresh-now)))
                    bms)
          (font-proc end))))))

; (animate-fonts font1 font2 font3 ...)
; the fonts should be ready for interpolation (see usemath example)
; They are interpolated in series: font1 ----> font2 ----> font3 ...
(define-syntax animate-fonts 
  (syntax-rules ()
    [(animate-fonts f ...)
     (let* ([ifp (n-groups (list f ...) 2)]
              [intps (map (lambda (p)
                           (let ([d (- (second p) (first p))]
                                 [a (first p)])
                             (lambda (i)
                               (+ a (* i d)))))
                          ifp)]
              [l (length intps)]
              [proc (lambda (i)
                      (let ([r (/ (remainder i 1000) 1000)]
                            [d (quotient i 1000)])
                        ((list-ref intps 
                                   (if (= d l) (- d 1) d)) 
                         (if (= d l) 1 r))))])
       (lambda ()
         (new-animate proc 0 (* 1000 l) ((curry +) 25))))]))
         

; Slider-Editor
; text String
; size Integer
; Inputs (Hash (Symbol . Integer))
; Representation of a 'slider editor'
; the values controlled by the sliders are stored in a HashTable
; the text to display and the size of it are in the field text and size
(struct slider-editor
  (text size inputs)
  #:transparent)

; SliderEditor Symbol Integer -> SliderEditor
; produce a new editor with the input updated
(define (update-inputs e f v)
  (struct-copy slider-editor e
               [inputs (hash-set (slider-editor-inputs e) f v)]))

; SliderEditor String -> SliderEditor
; produce a new editor with the text updated
(define (update-text e t)
  (struct-copy slider-editor e
               [text (string->text t)]))

; SliderEditor Integer -> SliderEditor
; produce a new editor with the size updated
(define (update-size e s)
  (struct-copy slider-editor e
               [size s]))

; SliderEditor Symbol -> Integer
; produce the value of the input f
(define (get-input e f)
  (hash-ref (slider-editor-inputs e) f))

; See below for an example
(define-syntax slider-application
  (syntax-rules (sliders update)
    [(_ [font-proc final-proc] 
        [sliders (sl-name sl-min sl-max init) ...]
        [text txt sz])
     (let ([w (world (slider-editor 
                      (string->text txt) sz 
                      (make-immutable-hash (list (cons 'sl-name init) ...))))]
           [f-proc font-proc]
           [fi-proc final-proc])
       (let* ([slider-updater (lambda (f v can)
                                  (begin
                                    (set-world-current-state! 
                                     w 
                                     (update-inputs (world-current-state w) f v))
                                    ;(print (world-current-state w))
                                    (send can refresh)))]
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
                               ((get-drawing-proc (apply f-proc is)) 
                                dc 1.2 (slider-editor-text e) (slider-editor-size e))))])]
                [t (new text-field%
                        [label "Text"]
                        [parent tf]
                        [init-value txt]
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
                                                (fi-proc
                                                   (apply f-proc 
                                                          (map ((curry get-input) (world-current-state w)) '(sl-name ...))))
                                                filepath
                                                #:overwrite #t))))])])          
           (let ([sl-name (new slider%
                               [parent sls]
                               [style '(horizontal vertical-label)]
                               [label (symbol->string 'sl-name)]
                               [min-value sl-min]
                               [max-value sl-max]
                               [init-value (get-input (world-current-state w) 'sl-name)]
                               [callback (lambda (s e) (slider-updater 'sl-name (send s get-value) can))])]
                 ...)
             (lambda ()
             (send frame show #t)))))]))
       


;(require "../../sfont-examples/fontwrite-square.rkt")
;
;(define (sq-proc weight width)
;    (sq #:weight (/ weight 1000) #:width (/ width 1000)))
;      
;(define main
;  (slider-application
;   [font sq-proc identity]
;   [sliders
;    (weight 0 1000 500)
;    (width 0 1000 500)]
;   [text "cabde o bacco" 100]))
;
;(main)

