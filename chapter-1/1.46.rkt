#lang planet neil/sicp

(define (iterative-improve improve guess)
  (define (good-enough? a b)
    (< (abs (- a b)) 0.0000000001))
  (define (iter guess-1 guess-2)
    (if (good-enough? guess-1 guess-2)
        guess-2
        (iter guess-2 (improve guess-2))))
  (iter guess (improve guess)))

(define (sqrt x)
  (iterative-improve (lambda (y)
                       (/ (+ y (/ x y)) 2))
                     1.0))

(define (fixed-point f guess)
  (iterative-improve f guess))

(sqrt 1025)
(fixed-point cos 1.0)