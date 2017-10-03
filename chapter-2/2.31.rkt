#lang planet neil/sicp
(define (square x)
  (* x x))

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

(define (tree-map-square-tree tree)
  (tree-map square tree))

;; treat as a tree
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;;treat as lists in lists
(define (my-square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? (car tree))) (cons (square (car tree)) (my-square-tree (cdr tree))))
        (else (cons (my-square-tree (car tree))
                    (my-square-tree (cdr tree))))))

(define (map-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-square-tree sub-tree)
             (square sub-tree)))
       tree))


(display (square-tree
        (list 1
              (list 2 (list 3 4) 5))))

(newline)

(display (tree-map-square-tree
        (list 1
              (list 2 (list 3 4) 5))))