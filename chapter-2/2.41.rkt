#lang planet neil/sicp

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triple n)
  (flatmap (lambda (k)
             (flatmap (lambda (j)
                        (map (lambda (i) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- k 1))))
           (enumerate-interval 1 n)))


(define (make-triple-sum triple)
  (list (car triple) (cadr triple) (caddr triple) (+ (car triple) (cadr triple) (caddr triple))))

(define (equal-sum-triple n s)
  (define (sum-equal? triple)
    (= (+ (car triple) (cadr triple) (caddr triple)) s))
  (map make-triple-sum
       (filter sum-equal?
               (unique-triple n))))

(display (unique-triple 5))

(newline)

(display (equal-sum-triple 6 9))