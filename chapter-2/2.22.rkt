#lang planet neil/sicp
(define (square x)
  (* x x))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

;change cons to append cause (cons list1 list2) != (append list1 list2)
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))

(square-list (list 1 2 3 4 5 6 7))
(square-list-iter (list 1 2 3 4 5 6 7))
