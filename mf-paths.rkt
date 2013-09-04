#lang racket
(require "vec.rkt"
         "bezier.rkt")
;(0; 3) - - - (0; 0) - - - (3; 0) is equivalent to
;(0; 3) : : controls (¡0:0002; 2:9998) and (¡0:0002; 0:0002)
;: : (0; 0) : : controls (0:0002;¡0:0002) and (2:9998;¡0:0002) : : (3; 0)

;;; metafont style paths (?)
(define-syntax mf~
  (syntax-rules (.. :: -- --- & cycle controls curl tension and atleast infinity)
    [(mf~ (x y) p ... cycle)
     (mf~ (x y) p ... (x y))]
    [(mf~) '()]
    [(mf~ (x y)) (list (vec x y))]
    [(mf~ (x y) -- (x1 y1) . r)
     (append (list (vec x y) (vec x y) (vec x1 y1))
             (mf~ (x1 y1) . r))]
    [(mf~ (x y) .. controls (cx cy) and (cx1 cy1) .. (x1 y1) . r)
     (append (list (vec x y) (vec cx cy) (vec cx1 cy1))
             (mf~ (x1 y1) . r))]))
          
          