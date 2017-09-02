#lang planet neil/sicp
(define (make-interval a b)
      (cons a b))

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

(define (mul-interval x y); x[a,b],y[c,d]
  (define (stat-one x)
    (and (< (lower-bound x) 0) (< (upper-bound x) 0)))
  (define (stat-two x)
    (and (< (lower-bound x) 0) (> (upper-bound x) 0)))
  (define (stat-three x)
    (and (> (lower-bound x) 0) (> (upper-bound x) 0)))
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (stat-one x) (stat-one y))
         (make-interval (* b d) (* a c)))
        ((and (stat-one x) (stat-two y))
         (make-interval (* a d) (* a c)))
        ((and (stat-one x) (stat-three y))
         (make-interval (* a d) (* b c)))
        ((and (stat-two x) (stat-one y))
         (make-interval (* b c) (* a c)))
        ((and (stat-two x) (stat-two y))
         (make-interval (min (* b c) (* a d)) (max (* a c) (* b d))))
        ((and (stat-two x) (stat-three y))
         (make-interval (* a d) (* b c)))
        ((and (stat-three x) (stat-one y))
         (make-interval (* b c) (* a d)))
        ((and (stat-three x) (stat-two y))
         (make-interval (* b c) (* b d)))
        ((and (stat-three x) (stat-three y))
         (make-interval (* a c) (* b d))))))
(define (div-interval x y)
  (define (cross-zero in)
    (< (* (lower-bound in) (upper-bound in)) 0))
  (if (cross-zero y)
      (display "div zero error")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (make-center-percent c p)
  (if (and (>= p 0) (<= p 1))
      (make-interval (* c (- 1 p)) (* c (+ 1 p)))
      (display "p must >=0 and <=1")))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percent x)
  (let ((c (center x)))
    (/ (- (upper-bound x) c) c)))

;c1*(1-p1) * c2*(1-p2) -- lower-bound of multi
;c1*(1+p1) * c2*(1+p2) -- upper-bound of multi
;=> 1-p3 = (1-p1)(1-p2)/k; 1+p3 = (1+p1)(1+p2)/k; c3 = c1*c2*k
;=> kp3 = p1 + p2 => k = 1 + p1p2
;=> p3 = (p1+p2)/(1+p1p2); k = 1 + p1p2
(define (mul-interval-per x y)
  (if (and (> (lower-bound x) 0) (> (lower-bound y) 0))
      (let ((c1 (center x))
            (p1 (percent x))
            (c2 (center y))
            (p2 (percent y)))
        (make-center-percent (* c1 c2 (+ 1 (* p1 p2))) (/ (+ p1 p2) (+ 1 (* p1 p2)))))
      (display "x.lower-bound and y.lower-bound must >0")))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (mul-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(par1 (make-interval 51 0.2) (make-interval 49 0.1))
(par2 (make-interval 51 0.2) (make-interval 49 0.1))
;Ator is right
