#lang planet neil/sicp
(define x (list (list 1 2) (list 3 4)))

(display x)

(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? (car x))) (append (list (car x)) (fringe (cdr x))))
        (else (append (fringe (car x)) (fringe (cdr x))))))


(display (fringe x))

(display (fringe (list x x)))