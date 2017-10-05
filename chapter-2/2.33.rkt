#lang planet neil/sicp
(define (square x)
  (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (acc-append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (acc-length sequence)
  (accumulate (lambda (x y)
                (+ y 1))
              0
              sequence))

(acc-map square (list 1 2 3 4))
(acc-append (list 1 2 3) (list 4 5 6))
(acc-length (list 1 2 3 4 5))
