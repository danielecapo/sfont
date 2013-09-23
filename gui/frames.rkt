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
                            ;(send canv set-canvas-background (send the-color-database find-color "white"))
                            (send canv refresh-now)
                            (aux (inc-proc c)))))])
        (begin 
          (send frame show #t)
          (aux (inc-proc start))))))


(define-syntax slider-application
  (syntax-rules (font sliders update)
    [(_ [font font-proc] 
        [sliders (sl-name sl-min sl-max) ...]
        [update update-proc])
     (lambda (state)
       (let* ([w (world state)]
              [frame (new frame%
                          [label "sfont"]
                          [width 1000]
                          [height 400])]
              [pan (new horizontal-pane%
                        [parent frame])]
              [sls (new vertical-pane%
                        [parent pan]
                        [min-width 300]
                        [stretchable-width #f])]
              [can (new canvas%
                        [parent pan]
                        [min-width 700]
                        [stretchable-width #t])])
         (let [(sl-name (new slider%
                             [parent sls]
                             [style '(horizontal vertical-label)]
                             [label (symbol->string 'sl-name)]
                             [min-value sl-min]
                             [max-value sl-max]))
               ...]
           (send frame show #t))))]))
     
                    
      
      

;(make-canvas frame (get-drawing-proc light))