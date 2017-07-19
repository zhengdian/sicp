#lang planet neil/sicp
;;filtered-accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter x result)
    (cond ((> x b) result)
          ((filter x) (iter (next x) (combiner result (term x))))
          (else (iter (next x) result))))
  (iter a null-value))

;;use accumulate define product
(define (product term a next b)
  (filtered-accumulate (lambda (x) true) * 1 term a next b))

(define (factorial n)
  (product (lambda (x) false) 1 (lambda (x) (+ x 1)) n))

(factorial 5)

;;use accumulate define sum
(define (sum term a next b)
  (filtered-accumulate (lambda (x) x) + 0 term a next b))

(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)