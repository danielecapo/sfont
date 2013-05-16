#lang racket

(require "font.rkt"
         "selectors.rkt")

(define (merge-to-layer target source layer-name)
  (let ([source-foreground (ufo:get-layer source)]
        [target-layers (dict-remove (ufo:font-layers target) layer-name)]
        [target-layer (ufo:get-layer target layer-name)])
    (struct-copy 
     ufo:font
     target
     [layers (append 
              target-layers 
              (list
               (if target-layer
                   (append (list layer-name (ufo:layer-info target-layer))
                           (let ([old-glyphs (ufo:layer-glyphs target-layer)]
                                 [new-glyphs (ufo:layer-glyphs source-foreground)])              
                             (foldl (lambda (g acc) (dict-set acc (car g) (cdr g)))
                                    old-glyphs
                                    new-glyphs)))
                   (append (list layer-name #f)
                           (ufo:layer-glyphs source-foreground)))))])))

(define (merge-to-background target source)
  (merge-to-layer target source 'public-background))

(define (subset f glyphs)
  (



