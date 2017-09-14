#lang planet neil/sicp
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch structure)
  (car structure))

(define (right-branch structure)
  (cdr structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight structure)
  (cond ((not (pair? structure)) structure)
        (else (+ (total-weight (branch-structure (left-branch structure)))
                 (total-weight (branch-structure (right-branch structure)))))))

(define (balance? structure)
  (cond ((not (pair? structure)) true)
        (else (let ((left-structure (branch-structure (left-branch structure)))
                    (right-structure (branch-structure (right-branch structure))))
                (and (= (* (total-weight left-structure) (branch-length (left-branch structure)))
                        (* (total-weight right-structure) (branch-length (right-branch structure))))
                     (balance? left-structure)
                     (balance? right-structure))))))

;                       6/         \3
;                       13     2/       \11
;                          7/    \4      4
;                          8     14
;

(define b1 (make-branch 7 8))
(define b2 (make-branch 4 14))
(define b3 (make-branch 11 4))
(define b4 (make-branch 6 13))
(define s1 (make-mobile b1 b2))
(define b5 (make-branch 2 s1))
(define s2 (make-mobile b5 b3))
(define b6 (make-branch 3 s2))
(define s3 (make-mobile b4 b6))

(total-weight s3)
(balance? s3)

; d> just change right-branch and branch-structure in a