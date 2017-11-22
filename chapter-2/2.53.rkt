#lang planet neil/sicp

(list 'a 'b 'c)

(list (list 'george))

(display (cdr '((x1 x2) (y1 y2))))

(display (cadr '((x1 x2) (y1 y2))))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))