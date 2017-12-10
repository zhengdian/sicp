#lang planet neil/sicp

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
            (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
    (copy-to-list tree '()))

(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (union-set set1 set2)
  (let ((list-set1 (tree->list-2 set1)))
    (define (add-list-to-set list set)
      (if (null? list)
          set
          (add-list-to-set (cdr list) (adjoin-set (car list) set))))
    (list-tree (tree->list-2 (add-list-to-set list-set1 set2)))))

(define (intersection-set set1 set2)
  (let ((list-set1 (tree->list-2 set1)))
    (define (intersect-list-set list result)
      (if (null? list)
          result
          (if (element-of-set? (car list) set2)
              (intersect-list-set (cdr list) (adjoin-set (car list) result))
              (intersect-list-set (cdr list) result))))
    (intersect-list-set list-set1 nil)))


(define a
  (make-tree 7
             (make-tree 3
                        (make-tree 1 nil nil)
                        (make-tree 5 nil nil))
             (make-tree 9
                        nil
                        (make-tree 21 nil nil))))

(define b
  (make-tree 17
             (make-tree 13
                        (make-tree 1 '() '())
                        (make-tree 15 nil nil))
             (make-tree 19
                        nil
                        (make-tree 21 nil nil))))

(display (union-set a b))
(newline)
(display (intersection-set a b))