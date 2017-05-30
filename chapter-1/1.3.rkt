#lang planet neil/sicp
(define (sum_of_bigger a b c)
  (cond ((and (< a b) (< a c)) (+ b c))
        ((and (< b a) (< b c)) (+ a c))
        ((and (< c a) (< c b)) (+ a b))))

(sum_of_bigger 83 43 66)