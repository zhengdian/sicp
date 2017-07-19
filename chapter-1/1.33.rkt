#lang planet neil/sicp
;;filtered-accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter x result)
    (cond ((> x b) result)
          ((filter x) (iter (next x) (combiner result (term x))))
          (else (iter (next x) result))))
  (iter a null-value))

;;use accumulate define product
(define (product term a next b)
  (filtered-accumulate (lambda (x) true) * 1 term a next b))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(factorial 5)

;;use accumulate define sum
(define (sum term a next b)
  (filtered-accumulate (lambda (x) x) + 0 term a next b))

(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)

;; add only prime
(define (fast-prime? n times)
  (define (fermat-test n)
    (define (try-it a)
      (define (expmod base exp m)
        (define (square x)
          (* x x))
        (cond ((= exp 0) 1)
              ((even? exp)
               (remainder (square (expmod base (/ exp 2) m)) m))
              (else
               (remainder (* base (expmod base (- exp 1) m)) m))))
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (add-only-prime a b)
  (define (prime? x)
    (fast-prime? x 4))
  (filtered-accumulate prime? + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b))

(add-only-prime 3 10)

;;GCD-add
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-product n)
  (filtered-accumulate (lambda (x) (= (gcd x n) 1)) * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(gcd-product 6)