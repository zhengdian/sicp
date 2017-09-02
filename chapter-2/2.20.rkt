#lang planet neil/sicp
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(define (length items)
  (define (length-iter _items count)
    (if (null? _items)
        count
        (length-iter (cdr _items) (+ 1 count))))
  (length-iter items 0))

(define (same-parity x . y)
  (define (parity? a b)
    (if (= (remainder a 2) (remainder b 2))
        true
        false))
  (define (same items)
    (if (null? items)
          (list)
          (append (if (parity? x (car items))
                      (list (car items))
                      (list))
                  (same (cdr items)))))
  (if (= (length y) 0)
      (list x)
      (append (list x) (same y))))

(same-parity 2 3 5 6 7)
(same-parity 1 2 3 5 6 7)