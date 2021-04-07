#lang racket
(provide (all-defined-out))

; problem 1
(define (sequence low high stride)
     (let ([start (- high (remainder (- high low) stride))])
    (letrec([aux (lambda (last acc) (let([next (- last stride)]) 
                          (if (< next low) acc (aux next (cons next acc)))))])
        (if (< high low) null (aux start (cons start null))))))
                                                 
; problem 2
(define (string-append-map xs suffix)
   (if (or (null? suffix) (null? xs)) xs (map (lambda (y) (string-append y suffix))  xs)))

; problem 3
(define (list-nth-mod xs n)
   (cond [(< n 0) (error "error: list-nth-mod : negative numbeer")]
         [(null? xs) (error "error: liar-nrh-mos: empty list")]
         [#t (car (list-tail xs (remainder n (length xs))))]))

; problem 4
(define (stream-for-n-steps s n)
   (letrec ([aux (lambda (acc m sprime) (if (= m 0) acc (aux (append acc (list (car (sprime)))) (- m 1) (cdr (sprime)))))])
          (if (= n 0) null (aux (list (car (s))) (- n 1) (cdr (s))))))

; problem 5
(define funny-number-stream 
    (letrec ([g (lambda (x) (if (= 0 (remainder x 5)) (- x) x))]
             [f (lambda (x) (cons (g x) (lambda () (f (+ x 1)))))])
             (lambda () (f 1))))

; problem 6
(define dan-then-dog
    (letrec ([g (lambda (x) (if (= 0 (remainder x 2)) "dog.jpg" "dan.jpg"))]
             [f (lambda (x) (cons (g x) (lambda () (f (+ x 1)))))])
             (lambda () (f 1))))

; problem 7
(define (stream-add-zero s)
    (letrec ([f (lambda(s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
             (lambda () (f s))))

; problem 8
(define (cycle-lists xs ys)
    (letrec ([f (lambda (n) (let ([a (list-nth-mod xs n)]
                                  [b (list-nth-mod ys n)])
                             (cons (cons a b) (lambda () (f (+ n 1))))))])
             (lambda () (f 0))))
        
; problem 9
(define (vector-assoc v vec)
    (let ([n (vector-length vec)])
        (letrec ([aux (lambda (m) (cond [(= n m) n]
                                        [(and (pair? (vector-ref vec m)) (equal? (car (vector-ref vec m)) v)) m]
                                        [#t (aux (+ m 1))]))]
                 [index (aux 0)])
                 (if (= index n) #f (vector-ref vec index)))))
; problem 10
(define (cached-assoc xs n)
    (let ([cache (make-vector n #f)]
          [next-empty 0])
      (lambda (v)
          (let* ([check (vector-assoc v cache)]
                [check2 (if check check (assoc v xs))])
            (if (and (not check) check2)
                (begin (vector-set! cache next-empty check2)
                (set! next-empty (remainder (+ next-empty 1) n))
                check2)
             check2)))))

; problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
      (letrec ([x e1]    
              [f (lambda () (if (< e2 x) (f) #t))])
        (f) )]))
         
             
