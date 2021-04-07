#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) (if (> low high) '() (append (list low) (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix) (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (let ([i (remainder n (length xs))])
  (if (> 0 n) (error "list-nth-mod: negative number")
      (if (= 0 (length xs)) (error "list-nth-mod: empty list")
          (car (list-tail xs i))))))

(define (stream-for-n-steps s n)
  (if (>= 0 n) '()
      (append (list (car (s))) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
         (letrec ([acc (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x) (lambda () (acc (+ x 1)))))])
           (lambda () (acc 1))))

(define dan-then-dog
  (letrec ([acc (lambda (x) (cons (if (= (remainder x 2) 0) "dan.jpg" "dog.jpg") (lambda () (acc (+ x 1)))))])
    (lambda () (acc 0))))

(define stream-add-zero
  (letrec ([acc (lambda (s) (cons (car (s)) (lambda () (acc (cdr (s))))))])
    (lambda (s) ((lambda () (acc s))))))

(define (cycle-lists xs ys)
  (letrec ([acc (lambda (xs ys idx) (cons (cons (list-nth-mod xs idx) (list-nth-mod ys idx)) (lambda () (acc xs ys (+ idx 1)))))])
    (lambda () (acc xs ys 0))))

(define (vector-assoc v vec)
  (letrec
      ([is_valid (lambda (v vec index)
                   (if (pair? (vector-ref vec index)) (if (equal? (car (vector-ref vec index)) v ) #t #f) #f))]
       [acc (lambda (v vec index)
              (if (equal? (vector-length vec) (+ index 1)) #f
                  (if (is_valid v vec index) (vector-ref vec index) (acc v vec (+ index 1))))
              )])
    (acc v vec 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n)]
           [index 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if (pair? ans) ans
                   (let ([new-ans (assoc v xs)])
                     (if (pair? new-ans)
                         (begin (vector-set! memo index new-ans)
                                (set! index (if (= index (- n 1)) 0 (+ 1 index)))
                                 new-ans) #f
                         )))))])
    f))

