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

(fast-multi 8 29)