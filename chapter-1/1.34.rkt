#lang planet neil/sicp
(define (f g)
  (g 2))

(f f)
;;(f f) = (f 2) = (2 2) = error 这里expected a procedure that can be applied to arguments