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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

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

(define (apply-generic op . args)
  (let [[type-tags (map type-tag args)]]
    (let [[proc (get op type-tags)]]
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let [[type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)]]
                (let [[t1->t2 (get-coercion type1 type2)]
                      [t2->t1 (get-coercion type2 type1)]]
                  (cond ((t1->t2)
                         (apply-generic op (t1->t2 a1) a2))
                        ((t2->t1)
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))

;;a> 当找不到相应操作时，死循环

;;b> 没有

;;c> 


(define (new-apply-generic op . args)
  (let [[type-tags (map type-tag args)]]
    (let [[proc (get op type-tags)]]
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let [[type1 (car type-tags)]
                    [type2 (cadr type-tags)]
                    [a1 (car args)]
                    [a2 (cadr args)]]
                (let [[t1->t2 (get-coercion type1 type2)]
                      [t2->t1 (get-coercion type2 type1)]]
                  (cond ((equal? type1 type2) (error "No method for equal type" (list op type-tags)))
                        ((t1->t2)
                         (apply-generic op (t1->t2 a1) a2))
                        ((t2->t1)
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))







