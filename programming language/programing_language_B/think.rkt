#lang racket

(define a 10)

(define (add-a x)
  (let ([a a])   ; 这里是function body, call 时才会evaluate.
    (+ a x)))

(set! a 20)


(define b 10)

(define add-b
  (let ([b b])    ; 这里的Let立即运行了。
    (lambda (x)   ; 这里开始是function body
       (+ b x))))

(set! b 20)