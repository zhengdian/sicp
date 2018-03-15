#lang planet neil/sicp
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

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


(put 'deriv '+ (lambda (operands var)
                 (make-sum (deriv (car operands) var)
                           (deriv (cadr operands) var))))

(put 'deriv '* (lambda (operands var)
                 (make-sum
                  (make-product (car operands)
                                (deriv (cadr operands) var))
                  (make-product (deriv (car operands) var)
                                (cadr operands)))))

(put 'deriv '^ (lambda (operands var)
                 (define (base exp)
                   (car operands))
                 (define (exponent exp)
                   (cadr operands))
                 (make-product (exponent exp)
                               (make-product (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    -1))
                                     (deriv (base exp)
                                            var)))))


(define (new-deriv exp var)
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp)) (operands exp)
                                      var))))

(display (deriv '(+ x 3) 'x))
(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
(display (deriv '(^ x (* 3 (+ x y))) 'x))

(newline)
(display (new-deriv '(+ x 3) 'x))
(newline)
(display (new-deriv '(* (* x y) (+ x 3)) 'x))
(newline)
(display (new-deriv '(^ x (* 3 (+ x y))) 'x))

;;;a>因为针对number 和 variable的操作是基本操作，不是基于被求导表达式类型的分派

;;; exchange 'derive with operator