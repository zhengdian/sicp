#lang planet neil/sicp

(define (make-queue)
  (let [[front-ptr '()]
        [rear-ptr '()]]
    (define (dispatch m)
      (define (empty-queue?) (null? front-ptr))
      (define (front-queue)
        (if (empty-queue?)
            (error "Front-queue called with an empty queue")
            (car front-ptr)))
      (define (print-queue)
        (display front-ptr)
        (newline))
      (define (insert! item)
        (let [[new-pair (cons item '())]]
          (cond ((empty-queue?)
                 (set! front-ptr new-pair)
                 (set! rear-ptr new-pair)
                 dispatch)
                (else
                 (set-cdr! rear-ptr new-pair)
                 (set! rear-ptr new-pair)
                 dispatch))))
      (define (delete-queue!)
        (cond ((empty-queue?)
               (error "Delete-queue! called with an empty queue"))
              (else
               (set! front-ptr (cdr front-ptr))
               dispatch)))
      
      (cond ((eq? m 'insert-queue!) insert!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'print-queue) print-queue)
            (else (error "dispatch m" m))))
    dispatch))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (print-queue queue)
  ((queue 'print-queue)))


(define q1 (make-queue))

(print-queue (insert-queue! q1 'a))

(print-queue (insert-queue! q1 'b))

(print-queue (delete-queue! q1))

(print-queue (delete-queue! q1))






