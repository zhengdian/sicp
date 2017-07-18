#lang planet neil/sicp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;之前的的错误在于F与f被当做同一个过程了，由此可知大小写不敏感
(define (my-integral f a b n)
  (define (Fu x)
    (cond ((or (= x n) (= x 0)) (f (+ a (* x (/ (- b a) n)))))
          ((= (remainder x 2) 1) (* 4 (f (+ a (* x (/ (- b a) n))))))
          (else (* 2 (f (+ a (* x (/ (- b a) n))))))))
  (define (next-k x)
    (+ x 1))
  (* (sum Fu 0 next-k n) (- b a) (/ 1 (* 3 n))))

(integral (lambda (x) (* x x x)) 0 1 0.00001)

(my-integral (lambda (x) (* x x x)) 0 1 100)
