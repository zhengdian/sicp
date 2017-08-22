#lang planet neil/sicp
(define (repeated f n)
  (lambda (x)
    (define (iter i result)
      (if (= i 0)
           result
           (iter (- i 1) (f result))))
    (iter n x)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x)
    (average x (f x))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square a)
  (* a a))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (root x n k) ;;使用k平均阻尼
  (fixed-point (repeated (average-damp (lambda (y) (/ x (fast-expt y (- n 1))))) k) 1.0))

(root 1288 8 1)










