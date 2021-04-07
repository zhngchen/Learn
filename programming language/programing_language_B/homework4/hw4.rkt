#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; #1 stride > 0
(define (sequence low high stride) 
    (cond [(< high low) null]
            [(= high low) (list low)]
            [#t (cons low (sequence (+ low stride) high stride))]))


; #2 
(define (string-append-map xs suffix)
    (map (lambda (x) (string-append x suffix)) xs))

; #3
(define (list-nth-mod xs n)
    (cond [(< n 0) (raise (error "list-nth-mod: negative number"))]
          [(null? xs)  (raise (error "list-nth-mod: empty list"))]
          [#t (let ([i (remainder n (length xs))])
                (car (list-tail xs i)))]))

; #4 n >= 0
(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (let ([pair (s)])
            (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

; #5
(define funny-number-stream 
    (letrec ([get-next (lambda (x) 
                     (let ([pre (+ (abs x) 1)])
                                (if (= (remainder pre 5) 0)
                                    (- pre)
                                    pre)))]
           [f (lambda (x) (cons x (lambda () (f (get-next x)))))])
        (lambda () (f 1))))

; #6
(define dan-then-dog 
    (letrec ([dan-stream (lambda () (cons "dan.jpg" dog-stream))]
             [dog-stream (lambda () (cons "dog.jpg" dan-stream))])
        dan-stream))

; #7
(define (stream-add-zero s)
    (letrec ([s-pair (s)])
        (lambda () (cons (cons 0 (car s-pair)) (stream-add-zero (cdr s-pair))))))

; #8 xs,ys is not empty
(define (cycle-lists xs ys)
    (letrec ([pairs (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)) )]
             [f (lambda (n) (cons (pairs n) (lambda () (f (+ n 1)))))])
        (lambda () (f 0))))

; #9
(define (vector-assoc v vec)
    (letrec ([len (vector-length vec)]
             [aux (lambda (n)
                          (cond [(= n len) #f]
                                [(pair? (vector-ref vec n)) 
                                    (letrec ([nth-element (vector-ref vec n)]) 
                                        (if (equal? v (car nth-element)) nth-element (aux (+ n 1))))]
                                [#t (aux (+ n 1))]))])
        (aux 0)))

; #10
(define (cached-assoc xs n)
    (letrec ([cache (make-vector n #f)]
             [pos 0]
             [f (lambda (v) 
                (letrec ([ans (vector-assoc v cache)])
                            (if ans
                                ans
                                (letrec ([new-ans (assoc v xs)])
                                            (if new-ans
                                                (begin
                                                    (vector-set! cache pos new-ans)
                                                    (set! pos (remainder (+ pos 1) n))
                                                    new-ans)
                                                #f)))))])
        f))

; #challenge
(define-syntax while-less
    (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
               [loop (lambda () 
                            (if (< e2 v1)
                            (loop)
                            #t))])
            (loop))]))


