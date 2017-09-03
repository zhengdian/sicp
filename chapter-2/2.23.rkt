#lang planet neil/sicp

(define (my-for-each f items)
  (define (iter things answer)
    (if (null? things)
        true
        (iter (cdr things) (f (car things)))))
  (iter items nil))

(for-each (lambda (x) (newline) (display x))
          (list 1 2 3 4))

(newline)
(display "my-for-each: ")
(my-for-each (lambda (x) (newline) (display x))
          (list 1 2 3 4))

