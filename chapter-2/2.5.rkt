#lang planet neil/sicp
(define (even? n)
  (= (remainder n 2) 0))

(define (square a)
  (* a a))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(define (get-factor-except x n);;let x doesn't contain factor of n
  (if (= (remainder x n) 0)
      (get-factor-except (/ x n) n)
      x))

(define (get-index num base) ;base ^ index = num
  (if (= num 1)
      0
      (+ 1 (get-index (/ num base) base))))

(define (car x)
  (get-index (get-factor-except x 3) 2))

(define (cdr x)
  (get-index (get-factor-except x 2) 3))

(car (cons 234 564))
(cdr (cons 234 564))