#lang planet neil/sicp
(define x (list (list 1 2) (list 3 4) (list  5 (list 7 8))))

(display x)

(newline)

(display (reverse x))

(newline)

(define (deep-reverse x)
  (map reverse (reverse x)))

(display (deep-reverse x))