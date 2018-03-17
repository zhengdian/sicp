#lang planet neil/sicp

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMGA" op))))
  dispatch)


(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(real-part (make-from-mag-ang 1 (/ 3.14 4)))
(imag-part (make-from-mag-ang 1 (/ 3.14 4)))
(magnitude (make-from-mag-ang 1 (/ 3.14 4)))
(angle (make-from-mag-ang 1 (/ 3.14 4)))
