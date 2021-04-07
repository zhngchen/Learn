#lang racket
(provide (all-defined-out))

;(define (my-if-bad e1 e2 e3) 
    ;(if e1 e2 e3))

;(define (factorial-bad x)
    ;(my-if-bad (= x 0)
                ;1
                ;(* x (factorial-bad x - 1))))

;(define (ones) (+ 0 1))

(define b 3)
(define (f x) (+ x b))
(set! b 5)
(define z (f 4))