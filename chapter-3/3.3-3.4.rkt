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
    (define (check&dispatch m pwd num) ;;将pwd-wrong-num写在这里面就不行，为什么？ —— 因为acc绑定到dispatch，而dispatch内部定义pwd-wrong-num则会每次被初始化
      (define call-the-cops (lambda (x) (display "call-the-cops") (newline)))
      (if (eq? pwd password)
          (cond ((eq? m 'withdraw) (withdraw num))
                ((eq? m 'deposit) (deposit num))
                (else (error "Unknown request -- MAKE_ACOUNT" m)))
          (begin (set! pwd-wrong-num (+ pwd-wrong-num 1))
                 (if (>= pwd-wrong-num 3)
                     (call-the-cops pwd)
                     "Incorrect password"))))
    check&dispatch))

(define acc (make-account 'zhengdian 1000))

(acc 'withdraw 'zhengdian 800)

(acc 'withdraw 'asd 100)

(acc 'withdraw 'asd 100)

(acc 'withdraw 'asd 100)
