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


(define (tan-cf-recur x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- 0 (* x x))))
             (lambda (i)
               (- (* 2 i) 1))
             k))

(define (tan-cf-iter x k)
  (cont-frac-iter (lambda (i)
                    (if (= i 1)
                        x
                        (- 0 (* x x))))
                  (lambda (i)
                    (- (* 2 i) 1))
                  50))

(tan-cf-recur (/ 3.14159 4) 50)
(tan-cf-iter (/ 3.14159 4) 50)
