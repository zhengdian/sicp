#lang planet neil/sicp
(define (cube-root x)
  (cube-root-iter 0 1 x))

(define (cube-root-iter old-guess new-guess x)
  (if (good-enough old-guess new-guess x)
      new-guess
      (cube-root-iter new-guess (improve new-guess x) x)))

(define (good-enough old-guess new-guess x)
  (< (abs (/ (- old-guess new-guess) x))
     0.000001))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(cube-root 8.00)