#lang planet neil/sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOSE_BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


(define (contain? tree char)
  (define (contain-l? list char)
    (cond ((null? list) false)
          ((eq? char (car list)) true)
          (else (contain-l? (cdr list) char))))
  (contain-l? (symbols tree) char))

(define (encode-symbol char tree)
  (define (encode bits current-branch)
    (if (leaf? current-branch)
        bits
        (if (contain? current-branch char)
            (if (contain? (left-branch current-branch) char)
                (encode (append bits '(0)) (left-branch current-branch))
                (encode (append bits '(1)) (right-branch current-branch)))
            (error "bad char" char))))
  (encode '() tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


;;generate huffman tree

(define (generate-huffman-tree pairs)
  (define (successive-merge leaf-set)
    (if (= (length leaf-set) 1)
        (car leaf-set)
        (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                      (cadr leaf-set))
                                      (cddr leaf-set)))))
  (successive-merge (make-leaf-set pairs)))


(define sample-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

(display (encode '(A D A B B C A) sample-tree))












