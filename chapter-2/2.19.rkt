#lang planet neil/sicp
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount)
  (define (cc amount coin-values)
    (define (no-more? value-list)
      (null? value-list))
    (define (first-denomination value-list)
      (car value-list))
    (define (except-first-denomination value-list)
      (cdr value-list))
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
           (+ (cc amount
                  (except-first-denomination coin-values))
              (cc (- amount (first-denomination coin-values))
                  coin-values)))))
  (cc amount us-coins))

(count-change 73)
;no influence