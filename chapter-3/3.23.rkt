#lang planet neil/sicp

(define (make-deque)
  (let [[front-ptr '()]
        [rear-ptr '()]]
    (define (dispatch m)
      (define (empty?) (null? front-ptr))
      (define (front-que)
        (if (empty?)
            (error "Front-queue called with an empty queue")
            (car front-ptr)))
      (define (rear-que)
        (if (empty?)
            (error "Rear-que called with an empty queue")
            (car rear-ptr)))
      (define (print-que)
        (define (print-iter p p_end)
          (if (not (null? p))
              (if (eq? p p_end)
                  (display (car p))
                  (begin (display (car p))
                         (display " ")
                         (print-iter (cdr p) p_end)))
              (display "")))
        (display "(")
        (print-iter front-ptr rear-ptr)
        (display ")")
        (newline))
      (define (rear-insert! item)
        (let [[new-pair (cons item '())]]
          (cond ((empty?)
                 (set! front-ptr new-pair)
                 (set! rear-ptr new-pair)
                 dispatch)
                (else
                 (set-cdr! rear-ptr new-pair)
                 (set! rear-ptr new-pair)
                 dispatch))))
      (define (front-insert! item)
        (let [[new-pair (cons item front-ptr)]]
          (cond ((empty?)
                 (set! front-ptr new-pair)
                 (set! rear-ptr new-pair)
                 dispatch)
                (else
                 (set! front-ptr new-pair)
                 dispatch))))
      (define (front-delete!)
        (cond ((empty?)
               (error "Delete-queue! called with an empty queue"))
              ((eq? front-ptr rear-ptr)
               (set! front-ptr '())
               (set! rear-ptr '())
               dispatch)
              (else
               (set! front-ptr (cdr front-ptr))
               dispatch)))
      (define (rear-delete!)
        (define (get-before)
          (define (iter p end_p)
            (if (eq? p end_p)
                end_p
                (if (eq? (cdr p) end_p)
                    p
                    (iter (cdr p) end_p))))
          (iter front-ptr rear-ptr))
        (cond ((empty?)
               (error "rear-Delete! called with an empty queue"))
              (else
               (let [[before (get-before)]]
                 (if (eq? before rear-ptr)
                     (begin (set! front-ptr '())
                            (set! rear-ptr '())
                            dispatch)
                     (begin (set! rear-ptr before)
                            dispatch))))))
      
      (cond ((eq? m 'front-que) front-que)
            ((eq? m 'rear-que) rear-que)
            ((eq? m 'front-insert!) front-insert!)
            ((eq? m 'rear-insert!) rear-insert!)
            ((eq? m 'front-delete!) front-delete!)
            ((eq? m 'rear-delete!) rear-delete!)
            ((eq? m 'empty?) empty?)
            ((eq? m 'print-queue) print-que)
            (else (error "dispatch m" m))))
    dispatch))

(define (rear-insert-que! queue item)
  ((queue 'rear-insert!) item))

(define (front-insert-que! queue item)
  ((queue 'front-insert!) item))

(define (front-delete-que! queue)
  ((queue 'front-delete!)))

(define (rear-delete-que! queue)
  ((queue 'rear-delete!)))

(define (print-queue queue)
  ((queue 'print-queue)))


(define q1 (make-deque))

(print-queue (front-insert-que! q1 'a))

(print-queue (rear-insert-que! q1 'b))

(print-queue (front-insert-que! q1 'c))

(print-queue (rear-delete-que! q1))

(print-queue (front-delete-que! q1))

(print-queue (front-delete-que! q1))






