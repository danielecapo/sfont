#lang racket/base

(require racket/contract/base
         racket/function
         (only-in gls
                  method
                  add-method)
         "vec.rkt"
         "bezier.rkt")

(provide
 (contract-out
  [bezier-group/c (-> any/c boolean?)]))

(define bezier-group/c (flat-named-contract 'bezier-group/c (listof bezier/c)))

;; Transformations
(add-method transform 
            (method ((b bezier-group/c) (m trans-mat?)) 
                    (map (curryr transform m) b)))

(add-method translate
            (method ((b bezier-group/c) (x number?) (y number?))
                    (map (curryr translate x y) b)))

(add-method scale
            (method ((b bezier-group/c) (fx number?))
                    (map (curryr scale fx) b)))

(add-method scale
            (method ((b bezier-group/c) (fx number?) (fy number?))
                    (map (curryr scale fx fy) b)))

(add-method rotate
            (method ((b bezier-group/c) (a number?))
                    (map (curryr rotate a) b)))

(add-method skew-x 
            (method ((b bezier-group/c) (a number?))
                    (map (curryr skew-x a) b)))
(add-method skew-y 
            (method ((b bezier-group/c) (a number?))
                    (map (curryr skew-y a) b)))

(add-method reflect-x 
            (method ((b bezier-group/c))
                    (map reflect-x b)))

(add-method reflect-y 
            (method ((b bezier-group/c))
                     (map reflect-y b)))


