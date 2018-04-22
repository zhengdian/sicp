#lang racket
#|
(define random-init 10)

(define (rand-update x)
  (let [[a 43176]
        [b 34227]
        [m 1117]]
    (remainder (+ (* a x) b) m)))

(define rand
  (let [[x random-init]]
    (lambda ()
      (set! x (rand-update x))
      x)))
|#

(define (P x y)
  (define (square x) (* x x))
  (<= (+ (square x) (square y)) 1))

(define (estimate-integral trials P x1 x2 y1 y2)
  (define (random-in-range low high)
    (let [[range (- high low)]]
      (+ low (random range))))
  (define (test)
    (let [[tx (random-in-range x1 x2)]
          [ty (random-in-range y1 y2)]]
      (P tx ty)))
  (if (and (<= x1 -1) (<= y1 -1) (>= x2 1) (>= y2 1))
      (let [[probability (monte-carlo trials test)]
            [rect-area (* (- x2 x1) (- y2 y1))]]
        (* probability rect-area))
      (error "hehe")))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(estimate-integral 10000000 P -1 1 -1 1)