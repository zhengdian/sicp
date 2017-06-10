#lang planet neil/sicp
(define (f-recursion n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        ((> n 2) (+ (f-recursion (- n 1))
                   (* 2 (f-recursion (- n 2)))
                   (* 3 (f-recursion (- n 3)))))))

(f-recursion 4)

(define (f-iter n)
  (define (f a b c count)
    (if (= count 0)
        c
        (f b c (+ c
                  (* 2 b)
                  (* 3 a)) (- count 1))))
  
 (cond ((= n 0) 0)
      ((= n 1) 1)
      ((>= n 2) (f 0 1 2 (- n 2) ))))

(f-iter 4)