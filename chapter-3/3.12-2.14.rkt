#lang planet neil/sicp

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let [[temp (cdr x)]]
          (display x)
          (display y)
          (set-cdr! x y)
          (display '=>)
          (display x)
          (newline)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

(newline)
(display w)

