#lang planet neil/sicp
(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (square x)
        (* x x))
      (define (divides? a b)
        (= (remainder b a) 0))
    
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time)) 
        false))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
    true)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes begin num)
  (if (> num 0) (search-for-primes (+ begin 2) (if (timed-prime-test begin) (- num 1)
                                                   num))))

 
(search-for-primes 100001 3)