#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjon-set x set)
  (if (element-of-set? x set)
      (set)
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((rest-union (union-set (cdr set1) set2)))
           (if (element-of-set? (car set1) rest-union)
               rest-union
               (cons (car set1) rest-union))))))


(display (intersection-set (list 1 2 3 4 5 9) (list 2 3 4 7 8)))
(newline)
(display (union-set (list 11 0 1 2 3 4 5 9) (list 2 3 4 7 8)))