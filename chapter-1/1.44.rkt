#lang planet neil/sicp
(define dx 0.1)

(define (repeated f n)
  (lambda (x)
    (define (iter i result)
      (if (= i 0)
           result
           (iter (- i 1) (f result))))
    (iter n x)))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (square x)
  (* x x))

((smooth square) 4)

(((repeated smooth 4) square) 4)