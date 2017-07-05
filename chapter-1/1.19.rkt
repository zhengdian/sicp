#lang planet neil/sicp
;a' = (a+b)q + ap
;b' = bp + aq
;a'' = (a' + b')q + a'p = (a+b)(2pq + q^2) + a(p^2 + q^2)
;b'' = b'p + a'q = b(p^2 + q^2) + a(2pq + q^2)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (true (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))
        ))

(fib 8)