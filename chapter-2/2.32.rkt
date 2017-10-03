#lang planet neil/sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (p-element (car s)))
        (append rest (map (lambda (x)
                            (cons p-element
                                  x))
                          rest)))))

(display (subsets (list 1 2 3)))
