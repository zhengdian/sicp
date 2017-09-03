#lang planet neil/sicp
(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

(square-list (list 1 2 3 4 5 6 7))
(square-list-map (list 1 2 3 4 5 6 7))