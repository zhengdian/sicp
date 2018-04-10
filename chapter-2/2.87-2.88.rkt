#lang planet neil/sicp

(define (make-table)
  (let [[local-table (list '*table*)]]
    (define (lookup key-1 key-2)
      (let [[subtable (assoc key-1 (cdr local-table))]]
        (if subtable
            (let [[record (assoc key-2 (cdr subtable))]]
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let [[subtable (assoc key-1 (cdr local-table))]]
        (if subtable
            (let [[record (assoc key-2 (cdr subtable))]]
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))


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

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))


(define (add . args) (apply apply-generic 'add args));;must add apply
(define (sub . args) (apply apply-generic 'sub args))
(define (mul . args) (apply apply-generic 'mul args))
(define (div . args) (apply apply-generic 'div args))

(define (ab-square x) (mul x x))
(define (ab-sqrt x) (apply-generic 'sqrt x))
(define (ab-cos x) (apply-generic 'cos x))
(define (ab-sin x) (apply-generic 'sin x))
(define (ab-atan x y) (apply-generic 'atan x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'sqrt '(scheme-number)
       (lambda (x) (sqrt x)))
  (put 'cos '(scheme-number)
       (lambda (x) (cos x)))
  (put 'sin '(scheme-number)
       (lambda (x) (sin x)))
  (put 'atan '(scheme-number scheme-number)
       (lambda (x y) (atan x y)))

  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'negative '(scheme-number)
       (lambda (x) (- x)))
  )

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let [[g (gcd n d)]]
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;;;interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'sqrt '(rational)
       (lambda (x) (let [[val (/ (numer x) (denom x))]]
                     (tag (make-rat (sqrt val) 1)))))

  (put 'sin '(rational)
       (lambda (x) (let [[val (/ (numer x) (denom x))]]
                     (tag (make-rat (sin val) 1)))))

  (put 'cos '(rational)
       (lambda (x) (let [[val (/ (numer x) (denom x))]]
                     (tag (make-rat (cos val) 1)))))

  (put 'atan '(rational rational)
       (lambda (x y) (let [[val-x (/ (numer x) (denom x))]
                           [val-y (/ (numer y) (denom y))]]
                     (tag (make-rat (atan val-x val-y) 1)))))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  )

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (install-rectangular-package)
  ;;internal procedures
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos a)) (mul r (sin a))))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (ab-sqrt (add (ab-square (real-part z))
                  (ab-square (imag-part z)))))
  (define (angle z)
    (ab-atan (imag-part z) (real-part z)))

  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  )

(define (install-polar-package)
  ;;internal procedures
  (define (square x) (mul x x))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (ab-cos (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (ab-sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (ab-sqrt (add (square x) (square y)))
          (atan y x)))

  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  )


(define (install-complex-package)
  ;;imported procedures from rectangular and polar packages
  (install-rectangular-package)
  (install-polar-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  ;;internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))

  ;;interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part 'complex
       (lambda (z) (real-part z)))

  (put 'imag-part 'complex
       (lambda (z) (imag-part z)))
  )

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part n)
  ((get 'real-part 'complex) (contents n)))

(define (imag-part n)
  ((get 'imag-part 'complex) (contents n)))


(define (install-int)
  ;;internal
  (define (add-int a b) (add a b))
  (define (sub-int a b) (sub a b))
  (define (mul-int a b) (mul a b))
  (define (div-int a b) (div a b))
  (define (sqrt-int x) (sqrt x))
  ;;interface
  (put 'add '(int int) add-int)
  (put 'sub '(int int) sub-int)
  (put 'mul '(int int) mul-int)
  (put 'div '(int int) div-int)
  (put 'sqrt '(int) sqrt-int)
  (put 'sin '(int)
       (lambda (x) (sin x)))
  (put 'cos '(int)
       (lambda (x) (cos x)))
  (put 'atan '(int int)
       (lambda (x y) (atan x y)))
  )

(define (make-int-number n)
  (cons 'int n))

(define (install-raise)
  ;;internal procedures
  (define (int->rational n)
    (make-rational (contents n) 1))

  (define (rational->scheme n)
    (let [[content (contents n)]]
       (/ (car content) (cdr content))))

  (define (scheme->complex n)
    (make-complex-from-real-imag (contents n) 0))

  ;;interface
  (put 'raise 'int int->rational)
  (put 'raise 'rational rational->scheme)
  (put 'raise 'scheme-number scheme->complex)

  (put 'project 'int (lambda (n) n))
  
  (put 'project 'rational (lambda (n)
                            (let [[content (contents n)]]
                              (make-int-number (round (/ (car content) (cdr content)))))))
  
  (put 'project 'scheme-number (lambda (n)
                                 (make-rational (contents n) 1)))

  (put 'project 'complex (lambda (n)
                            (real-part n)))

  (put 'type-level 'int 0)
  (put 'type-level 'rational 1)
  (put 'type-level 'scheme-number 2)
  (put 'type-level 'complex 3))

(define (raise n)
  (let [[raise-op (get 'raise (type-tag n))]]
    (if raise-op
        (raise-op n)
        (error "No raise op for " n))))

(define (type-level n)
  (let [[type-level-op (get 'type-level (type-tag n))]]
    (if type-level
        type-level-op
        (error "No such type-level" n))))

(define (project n)
  (let [[op (get 'project (type-tag n))]]
    (if op
        (op n)
        (error "No project for " n))))
(define (can-drop n)
    (equal? n
            (raise (project n))))

(define (drop n)
  (if (can-drop n)
      (drop (project n))
      n))


(define (apply-generic op . args)
  (let [[type-tags (map type-tag args)]]
    (cond ((= (length args) 1)
           (let [[proc (get op type-tags)]]
             (if proc
                 (apply proc (map contents args))
                 (error "No methods" (list op type-tags)))))
          ((= (length args) 2)
           (let [[proc (get op type-tags)]]
             (if proc
                 (apply proc (map contents args))
                 (let [[level-type1 (type-level (car args))]
                       [level-type2 (type-level (cadr args))]
                       [a1 (car args)]
                       [a2 (cadr args)]]
                   (cond ((< level-type1 level-type2)
                          (apply-generic op (raise a1) a2))
                         ((< level-type2 level-type1)
                          (apply-generic op a1 (raise a2)))
                         (else (error "No method for two same types" (list op type-tags))))))))
          ((> (length args) 2)
           (let [[front-two (apply-generic op (car args) (cadr args))]]
             (apply apply-generic op (cons front-two (cddr args)))))
          (else ("No method for these types" (list op type-tags))))))

(install-int)
(install-rational-package)
(install-scheme-number-package)
(install-complex-package)
(install-raise)

;;2.5.3

(define (install-polynomial-package)
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (poly=zero? poly)
    (define (zero? terms)
      (if (= (length terms) 0)
          true
          (and (=zero? (first-term terms)) (rest-terms terms))))
    (zero? (term-list poly)))

  (define (poly-negative poly)
    (make-polynomial (variable poly)
                     (negative-term-list (term-list poly))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD_POLY"
               (list p1 p2))))
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (negative-term-list (term-list p2))))
        (error "Polys not in same var -- SUB_POLY"
               (list p1 p2))))
#|
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL_POLY"
               (list p1 p2))))
|#
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let [[t1 (first-term L1)]
                 [t2 (first-term L2)]]
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2))) ; not add-poly but add, this will tag coeff which is polynomial
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  ;;interface
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? '(polynomial) poly=zero?)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'negative '(polynomial) poly-negative)
  #|
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2)))) |#
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  )

(define (=zero? x) (apply-generic '=zero? x))
(define (negative x) (apply-generic 'negative x))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;;term-list
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (negative-termlist term-list)
  (define (term-list-negative terms result)
      (if (empty-termlist? terms)
          result
          (term-list-negative (rest-terms terms)
                              (let [[negative-term (make-term (order (first-term terms))
                                                             (negative (coeff (first-term terms))))]]
                                (append result (list negative-term))))))
  (term-list-negative term-list '()))

(define (negative-term-list term-list) ;; use map implement will be more concise
  (map (lambda (term)
         (make-term (order term)
                    (negative (coeff term))))
       term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))


(install-polynomial-package)
(define t1 '((5 3) (4 2) (2 2) (1 9)))
(define p1 (make-polynomial 'x t1))
(display p1)

(newline)
(define t2 '((7 2) (6 0) (5 10) (4 1) (2 0) (1 1)))
(define p2 (make-polynomial 'x t2))
(display p2)

(newline)
(display (add p1 p2))
(newline)
(display (sub p1 p2))