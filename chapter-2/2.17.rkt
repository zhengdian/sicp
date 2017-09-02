#lang planet neil/sicp
(define one-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter _items count)
    (if (null? _items)
        count
        (length-iter (cdr _items) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(last-pair one-four)
