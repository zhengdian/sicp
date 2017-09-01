#lang planet neil/sicp
(define (make-interval a b) (cons a b))

(define (upper-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (if (> a b)
        a
        b)))

(define (lower-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (if (< a b)
        a
        b)))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(mul-interval (make-interval 1 8) (make-interval 3 5));7 * 2 != 37
(div-interval (make-interval 1 8) (make-interval 3 5));7 / 2 != 2.4
