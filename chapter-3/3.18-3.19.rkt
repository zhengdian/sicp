#lang planet neil/sicp

;3.18
(define check-ring
  (let [[passed '()]]
    (lambda (x)
      (define (check-passed x passed-list)
         (cond ((null? passed-list) false)
               ((eq? x (car passed-list)) true)
               (else
                (check-passed x (cdr passed-list)))))
      (cond ((not (pair? x)) false)
        ((check-passed x passed) true)
        (else (begin (set! passed (cons x passed))
                     (check-ring (cdr x))))))))

;(check-ring x)


;3.19 http://community.schemewiki.org/?sicp-ex-3.19
;由于需要判断的是list而不是树，只需考虑两个指针，一个以1为步长，一个以2为步长，如果有环，这两个指针必定相遇，如果无环，两者只能在list尾部nil处相遇

 (define (contains-cycle? lst) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter a b) 
     (cond ((not (pair? a)) #f) 
           ((not (pair? b)) #f) 
           ((eq? a b) #t) 
           ((eq? a (safe-cdr b)) #t) 
           (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
   (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))
  
  
 ; Tested with mzscheme implementation of R5RS: 
 (define x '(1 2 3 4 5 6 7 8)) 
 (define y '(1 2 3 4 5 6 7 8)) 
 (set-cdr! (cdddr (cddddr y)) (cdddr y)) 
 (define z '(1)) 
 (set-cdr! z z) 
 x ; (1 2 3 4 5 6 7 8) 
 y ; (1 2 3 . #0=(4 5 6 7 8 . #0#)) 
 z ; #0=(1 . #0#) 
 (contains-cycle? x) ; #f 
 (contains-cycle? y) ; #t 
 (contains-cycle? z) ; #t 


