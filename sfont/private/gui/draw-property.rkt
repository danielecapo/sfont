#lang racket/base

(provide (all-defined-out))

(define-values (prop:draw draw? draw-ref)
  (make-struct-type-property 'draw))

(define (get-drawing-proc o)
  (if (draw? o)
      ((draw-ref o) o)
      (error "I cant't draw this object")))