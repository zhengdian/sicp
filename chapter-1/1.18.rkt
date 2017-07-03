#lang planet neil/sicp

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (fast-multi a (halve b))))
        (true (+ a (fast-multi a (- b 1))))))

(define (fast-multi-iter a b)
  (define (f-m-i a b c)
    (cond ((= b 0) 0)
          ((= b 1) (+ a c))
          ((even? b) (f-m-i (double a) (halve b) c))
          (true (f-m-i a (- b 1) (+ a c)))))
  (f-m-i a b 0))

(fast-multi 8 39)
(fast-multi-iter 8 39)