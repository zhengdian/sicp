#lang planet neil/sicp
;;f = 1 / (1 + f) => 1/f = 1 + f => 1/f is gold
(define (cont-frac n d k)
  (define (cont-frac-recur n d i)
    (if (= k i)
        (/ (n i)
           (d i))
        (/ (n i)
           (+ (d i) (cont-frac-recur n d (+ i 1))))))
  (cont-frac-recur n d 1))

(define (cont-frac-iter n d k)
  (define (frac-iter n d i ret)
    (cond ((= 0 i) ret)
          (else (frac-iter n d (- i 1) (/ (n i) (+ (d i) ret))))))
  (frac-iter n d k 0))

(/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                50))


(/ 1 (cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                50))