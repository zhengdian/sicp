#lang planet neil/sicp
(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; gold^2 = gold + 1 => gold = 1 + 1/gold
(define (gold)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(gold)