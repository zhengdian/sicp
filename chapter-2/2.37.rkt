#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate (lambda (x y)
                                              (cons (car x) y))
                                            nil
                                            seqs))
            (accumulate-n op init (accumulate (lambda (x y)
                                                (cons (cdr x) y))
                                              nil
                                              seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (define (inner-product v w)
    (accumulate + 0 (map * v w)))
  (map (lambda (mi)
         (inner-product mi v))
       m))


(define (transpose mat)
  (accumulate-n cons nil mat)); 注意操作符是cons而不是list，为什么？

(define (matrix-*matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi)
           (matrix-*-vector cols mi)) m)))

(dot-product (list 1 2 3 4) (list 5 6 7 8))
(display (matrix-*-vector (list (list 1 4 7) (list 2 5 8) (list 3 6 9)) (list 1 2 3)))
(newline)
(display (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))
(newline)
(display (matrix-*matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))