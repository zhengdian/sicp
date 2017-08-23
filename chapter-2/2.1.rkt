#lang planet neil/sicp
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (> (/ d g) 0)
        (cons (/ n g) (/ d g))
        (cons (/ n (- g)) (/ d (- g))))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(mul-rat (make-rat 3 -3) (make-rat 1 3))