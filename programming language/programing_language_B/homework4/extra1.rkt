#lang racket
(provide (all-defined-out))

(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (let ([pair (s)])
            (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
; #1
(define (palindromic xs)
    (let ([ys (reverse xs)])
        (map (lambda (x y) (x + y)) xs ys)))

; #2
(define fibonacci 
    (letrec ([f (lambda (a b) (cons a (lambda () (f b (+ a b)))))])
        (lambda () (f 0 1))))


; #3
(define (stream-until f s)
    (let ([next (s)])
        (if (f (car next))
            (stream-until f (cdr next))
            (car (s)))))



