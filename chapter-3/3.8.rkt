#lang planet neil/sicp

(define f
  (let [[first-call? true]
        [value -1]]
    (lambda (x) (if first-call?
                    (begin (set! first-call? false)
                           (set! value (/ x 2))
                           value)
                    value))))

(f 1)
(f 0)
(f 1)
(f 2)
