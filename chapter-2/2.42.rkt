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


(define empty-board nil) ;; (queen-cols 0) = (()) ，如果是()，flatmap将不能正常工作

(define (val pos seq)
  (cond ((= pos 1) (car seq))
        (else (val (- pos 1) (cdr seq)))))

(define (safe? k sequence)
  (define (pos-val pos seq)
    (cond ((= pos 1) (car seq))
          (else (pos-val (- pos 1) (cdr seq)))))
  (define (not-eq a b)
    (not (= a b)))
  (let ((k-val (pos-val k sequence)))
    (define (pos-safe pos)
      (let ((p-val (pos-val pos sequence)))
        (and (not-eq k-val p-val)
             (not-eq k-val (+ p-val (- k pos)))
             (not-eq k-val (- p-val (- k pos))))))
    (define (safe-iter pos)
      (if (= pos 0)
          true
          (and (pos-safe pos) (safe-iter (- pos 1)))))
    (safe-iter (- k 1))))

(define (adjoin-position row k rest-queens-sequence)
  (if (= (- k 1) (length rest-queens-sequence))
      (append rest-queens-sequence (cons row nil))))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(display (queens 8))








