#lang planet neil/sicp

(define (make-account password balance)
  (let [[pwd-wrong-num 0]]
    (define (withdraw num)
      (if (>= balance num)
          (begin (set! balance (- balance num))
                 balance)
          "Insufficient funds"))
    (define (deposit num)
      (set! balance (+ balance num))
      balance)
    (define (check&dispatch m pwd num)
      (define call-the-cops (lambda () (display "call-the-cops") (newline)))
      (if (eq? pwd password)
          (cond ((eq? m 'withdraw) (withdraw num))
                ((eq? m 'deposit) (deposit num))
                (else (error "Unknown request -- MAKE_ACOUNT" m)))
          (begin (set! pwd-wrong-num (+ pwd-wrong-num 1))
                 (if (>= pwd-wrong-num 3)
                     (call-the-cops)
                     "Incorrect password"))))
    check&dispatch))

(define (make-joint acc acc-pwd sub-pwd)
  (define (dispatch m pwd num)
    (if (eq? pwd sub-pwd)
        (acc m acc-pwd num)
        "Incorrect sub count password"))
  dispatch)

(define acc (make-account 'zhengdian 1000))

(acc 'withdraw 'zhengdian 800)

(define zk-acc (make-joint acc 'zhengdian 'zd))

(zk-acc 'withdraw 'zd 150)

(zk-acc 'deposit 'zd 700)

