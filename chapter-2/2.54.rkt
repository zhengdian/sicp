#lang planet neil/sicp

(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) true)
        ((or (null? list1) (null? list2)) false)
        (else (let ((item1 (car list1))
                    (item2 (car list2))
                    (rest1 (cdr list1))
                    (rest2 (cdr list2)))
                (cond ((and (pair? item1) (pair? item2))
                       (and (equal? item1 item2)
                            (equal? rest1 rest2)))
                      ((and (not (pair? item1)) (not (pair? item2)))
                       (and (eq? item1 item2)
                            (equal? rest1 rest2)))
                      (else false))))))

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))