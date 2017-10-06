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

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(display (fold-left list nil (list 1 2 3)))
(display (fold-right list nil (list 1 2 3)))

;;op 需要满足的性质：满足交换律和结合律