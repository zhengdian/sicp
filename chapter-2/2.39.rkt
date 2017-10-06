#lang planet neil/sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fr-reverse sequence)
  (fold-right (lambda (x y) (append y (cons x nil))) nil sequence))

(define (fl-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(display (fr-reverse (list 1 2 3 4 5 6)))
(display (fl-reverse (list 1 2 3 4 5 6)))
