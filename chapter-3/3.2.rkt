#lang planet neil/sicp

(define (make-monitor f)
  (let [[call-num 0]]
    (define (reset m) ;; 必须带参数，写成reset不对
      (set! call-num 0)
      (display "reset")
      (newline))
    (define how-many-calls? call-num)
    (define (call-f m)
      (set! call-num (+ call-num 1))
      (f m))
    (define (mf m)
      (cond ((eq? m 'reset-count) (reset m))
            ((eq? m 'how-many-calls?) call-num)
            (else (call-f m))))
    mf))

(define s (make-monitor sqrt))

(s 100)

(s 625)

(s 'how-many-calls?)

(s 'reset-count)

(s 'how-many-calls?)

(s 49)

(s 'how-many-calls?)