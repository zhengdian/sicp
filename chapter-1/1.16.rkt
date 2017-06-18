#lang planet neil/sicp
(define (even? n)
  (= (remainder n 2) 0))

(define (square a)
  (* a a))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (fast-expt-iter b n temp-val)  
  (cond ((= n 0) 1)  
    ((= n 1) (* b temp-val))  
    ((even? n) (fast-expt-iter (square b)  (/ n 2) temp-val))  
    (else (fast-expt-iter b (- n 1) (* temp-val b)))))

(fast-expt 2 11)
(fast-expt-iter 2 11 1)