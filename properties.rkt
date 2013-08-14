#lang racket

(require racket/generic)

(provide (all-defined-out))


;(define-values (prop:transform transform? transform-ref)
;  (make-struct-type-property 'transform))
      



  
;;; property prop:has-matrix  
;;; Used for objects that have transformation matrix (like components)

(define-values (prop:has-matrix has-matrix? has-matrix-ref)
  (make-struct-type-property 'has-matrix))

; Any -> TransfromationMatrix
; produce the TransformationMatrix of the object (if the (has-matrix? o) is true)

(define (get-matrix o)
  (if (has-matrix? o)
      ((has-matrix-ref o) o)
      (error "Object has no matrix")))

; id Object(T) TransfromationMatrix -> Object(T)
; produce an object of type id with the new transformation matrix

(define-syntax-rule (set-matrix id o m)
  (if (has-matrix? o)
      (struct-copy id o [matrix m])
      (error "Object has no matrix")))

;;; property prop:has-position  
;;; Used for objects that have a position

(define-values (prop:has-position has-position? has-position-ref)
  (make-struct-type-property 'has-position))

; Any -> Vec
; produce the position Vec(tor) of the object

(define (get-position o)
  (if (has-position? o)
      ((has-position-ref o) o)
      (error "Object has no position")))

; id Object(T) Vec -> Object(T)
; produce an object of type id with the new position

(define-syntax-rule (set-position id o m)
  (if (has-position? o)
      (struct-copy id o [pos m])
      (error "Object has no position")))