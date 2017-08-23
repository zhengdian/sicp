#lang planet neil/sicp
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

(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (let ((re (remainder i 3)))
                    (cond ((= 2 re) (* (+ (/ (- i 2) 3) 1) 2))
                          (else 1.0))))
                50))

(+ 2 (cont-frac-iter (lambda (i) 1.0)
                     (lambda (i)
                       (let ((re (remainder i 3)))
                         (cond ((= 2 re) (* (+ (/ (- i 2) 3) 1) 2))
                               (else 1.0))))
                     50))
