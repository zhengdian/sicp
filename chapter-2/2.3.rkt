#lang planet neil/sicp

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-rect p1 p2)
  (cons p1 p2))

(define (left-top-point rect)
  (car rect))

(define (right-bottom-point rect)
  (cdr rect))

(define (width rect)
  (abs (- (x-point (left-top-point rect)) (x-point (right-bottom-point rect)))))

(define (height rect)
  (abs (- (y-point (left-top-point rect)) (y-point (right-bottom-point rect)))))

(define (perimeter rect)
  (* 2 (+ (width rect) (height rect))))

(define (area rect)
  (* (width rect) (height rect)))

(perimeter (make-rect (make-point 3 5) (make-point 10 3)))
(area (make-rect (make-point 3 5) (make-point 10 3)))

(define (make-rect2 p1 width height)
  (cons p1 (cons width height)))

(define (width2 rect)
  (car (cdr rect)))

(define (height2 rect)
  (cdr (cdr rect)))

(define (perimeter2 rect)
  (* 2 (+ (width2 rect) (height2 rect))))

(define (area2 rect)
  (* (width2 rect) (height2 rect)))

(perimeter2 (make-rect2 (make-point 3 5) 7 2))
(area2 (make-rect2 (make-point 3 5) 7 2))