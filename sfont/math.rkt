#lang racket/base

;;; used only to simplify the use of math

(require "private/fontmath/math.rkt"
         "private/fontmath/interpolables.rkt")

(provide (all-from-out "private/fontmath/math.rkt"
                       "private/fontmath/interpolables.rkt"))
