#lang planet neil/sicp
(define test-list (list 1 4 9 16 25))

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

(define (reverse items)
  (if (= (length items) 1)
      items
      (append (reverse (cdr items)) (list (car items)))))

(reverse test-list)
