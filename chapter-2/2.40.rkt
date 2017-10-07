#lang planet neil/sicp

(define (prime? n times)
  (define (fermat-test n)
    (define (try-it a)
      (define (expmod base exp m)  
        (cond ((= exp 0) 1)  
              ((even? exp)  
               (check-nontrivial-sqrt (expmod base (/ exp 2) m) m)) ;; look here  
              (else  
               (remainder (* base (expmod base (- exp 1) m)) m))))  
      (define (check-nontrivial-sqrt n m)
        (define (square x)
          (* x x))
        (let ((x (remainder (square n) m)))  
          (if (and (not (= n 1))  
                   (not (= n (- m 1)))  
                   (= x 1))  
              0  
              x)))
      (= (expmod a (- n 1) n) 1))
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) true)
        ((fermat-test n) (prime? n (- times 1)))
        (else false)))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair)) 5))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (unique-pairs-prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(display (prime-sum-pairs 6))
(newline)
(display (unique-pairs-prime-sum-pairs 6))
