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
        (iter (next x) (combiner result x))))
  (iter a null-value))


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
