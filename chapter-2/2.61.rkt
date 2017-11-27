#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (define (adjoin-iter pre-set post-set)
    (cond ((null? post-set) (append pre-set (list x)))
          ((= x (car post-set)) (append pre-set post-set))
          ((< x (car post-set)) (append pre-set (cons x post-set)))
          (else (adjoin-iter (append pre-set (list (car post-set))) (cdr post-set)))))
  (adjoin-iter '() set))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        (cdr set2))))
                      ((< x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        set2)))
                      ((< x2 x1)
                       (cons x2
                             (union-set set1
                                        (cdr set2)))))))))

(adjoin-set 4 (list 1 2 3 5 6 7))
(adjoin-set 3 '())
(adjoin-set 6 (list 1 2 3 4 5))

(union-set (list 1 3 4 5 6 7 9) (list 0 2 4 5 7 8 9 10))
