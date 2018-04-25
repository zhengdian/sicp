#lang planet neil/sicp

(define (Ben-count-pairs x)
  (if (not (pair? x))
      0
      (+ (Ben-count-pairs (car x))
         (Ben-count-pairs (cdr x))
         1)))

;3.16
(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons 'e 'f))

(set-car! x y)
(set-cdr! x y)
(set-car! y z)
(set-cdr! y z)
;(set-cdr! z x) 成环
(Ben-count-pairs x)

;3.17
(define (count-pairs x)
  (define passed '())
  (define (check-passed x passed-list)
    (cond ((null? passed-list) false)
          ((eq? x (car passed-list)) true)
          (else
           (check-passed x (cdr passed-list)))))
  (define (count x passed-list)
    (if (not (pair? x))
      0
      (begin (set! passed (cons x passed))
             (let [[left-num (count (car x) passed)]
                   [right-num (count (cdr x) passed)]]
               (if (check-passed x passed-list)
                   (+ left-num
                      right-num)
                   (+ left-num
                      right-num
                      1))))))
  (count x passed))


(count-pairs x)
