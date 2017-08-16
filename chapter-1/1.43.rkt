#lang planet neil/sicp
(define (repeated f n)
  (lambda (x)
    (define (iter i result)
      (if (= i 0)
           result
           (iter (- i 1) (f result))))
    (iter n x)))

(define (square x)
  (* x x))

((repeated square 2) 5)