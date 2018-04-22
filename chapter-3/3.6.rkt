#lang planet neil/sicp

(define random-init 10)

(define (rand-update x)
  (let [[a 43176]
        [b 34227]
        [m 1117]]
    (remainder (+ (* a x) b) m)))

(define rand
  (let [[x random-init]]
    (lambda (m . n)
      (cond ((eq? m 'reset) (begin (set! x (car n))
                                   (display "reset")
                                   x))
            ((eq? m 'generate) (begin (set! x (rand-update x))
                                      x))
            (else (error "no such method"))))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'reset 47)
(rand 'generate)
(rand 'generate)
(rand 'generate)
