#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (p next)
                (if (pair? p)
                    (+ (count-leaves p) next)
                    (+ 1 next)))
              0
              t))

(define (count_leaves t)
    (accumulate + 0 (map (lambda (t)
                           (if (pair? t)
                               (count_leaves t)
                               1)) t)))

(count-leaves (list 1 2 3 (list 1 2 (list 2 3)) 4 ))
(count_leaves (list 1 2 3 (list 1 2 (list 2 3)) 4 ))