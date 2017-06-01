#lang planet neil/sicp
(define (sqrt x)
  (sqrt-iter 0 1.0 x))

(define (sqrt-iter old-guess new-guess x)
  (if (good-enough? old-guess new-guess x)
      new-guess
      (sqrt-iter new-guess (improve new-guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess )))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old-guess new-guess x)
  (< (abs (/ (- old-guess new-guess) x)) 0.001))

(define (square x) (* x x))

(sqrt 0.01)
