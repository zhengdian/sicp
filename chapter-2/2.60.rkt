#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjon-set x set)
  (cons x set))

(define (unique-set set)
  (if (null? set)
      '()
      (if (element-of-set? (car set) (cdr set))
          (unique-set (cdr set))
          (cons (car set) (unique-set (cdr set))))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

(display (unique-set (list 2 3 2 1 3 2 2)))
(newline)
(display (intersection-set (list 2 3 9 1 2 3 4 5 9) (list 4 8 2 2 3 4 7 8)))
(newline)
(display (unique-set (union-set (list 2 1 3 1 2 3 4 5 9) (list 7 8 3 2 3 4 7 8))))
