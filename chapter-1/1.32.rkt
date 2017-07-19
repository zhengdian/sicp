#lang planet neil/sicp
(define (accumulate-recurve combiner null-value term a next b)
  (define (recurve x)
    (if (> x b)
        null-value
        (combiner (term x)
                  (recurve (next x)))))
  (recurve a))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner result (term x)))))
  (iter a null-value))

;;use accumulate define product
(define (product-recurve term a next b)
  (accumulate-recurve * 1 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (factorial-recurve n)
  (product-recurve (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (factorial-iter n)
  (product-recurve (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(factorial-recurve 5)
(factorial-iter 5)

;;use accumulate define sum
(define (sum-recurve term a next b)
  (accumulate-recurve + 0 term a next b))

(sum-recurve (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)