#lang planet neil/sicp

(define (make-accumulator val)
  (lambda (x)
    (set! val (+ x val))
    val))

(define A (make-accumulator 5))

(A 10)

(A 7)