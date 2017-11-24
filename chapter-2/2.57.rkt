#lang planet neil/sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-sum a1 a2 . rest)
  (let ((two-sum (list '+ a1 a2)))
    (cond ((sum? a2) (append (list '+ a1) (cdr a2) rest))
          (else (append two-sum rest)))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (cond ((= (length s) 3) (caddr s))
        (else (cons '+ (cddr s)))))


(define (make-product m1 m2 . rest)
  (let ((two-product (list '* m1 m2)))
    (cond ((product? m2) (append (list '* m1) (cdr m2) rest))
          (else (append two-product rest)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (cond ((= (length p) 3) (caddr p))
        (else (cons '* (cddr p)))))


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '^ base exponent))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    -1))
                                     (deriv (base exp)
                                            var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(display (deriv '(+ x 3 x 4 x) 'x))
(newline)
(display (deriv '(* x y (+ x 3)) 'x));结果正确但没有化成最简形式



