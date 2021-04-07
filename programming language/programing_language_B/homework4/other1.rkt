#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (define (f low high stride lstacc)
    (let ([next (+ low stride)])
      (if (> next high)
        lstacc
        (f next high stride (append lstacc (list next))))))
  (if (< high low)
      empty
  (f low high stride (list low))))
  

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s n acc lst)
                (let ([pr (s)])
                  (if (= acc n)
                      lst
                      (f (cdr pr) n (+ acc 1) (append lst (list (car pr)))))))])
    (f s n 0 empty)))

(define funny-number-stream
   (letrec ([validate (lambda (x) (if (= (remainder x 5) 0)
                                      (* x -1)
                                      x))]
            [create (lambda (x) (cons x (lambda () (create  (validate (+ (abs x) 1))))))])
     (lambda () (create 1))))

(define dan-then-dog
   (letrec ([dan "dan.jpg"]
            [dog "dog.jpg"]
            [create (lambda (s)
                      (cons s (lambda () (create (if (string=? s dan)
                                                     dog
                                                     dan)))))])
     (lambda () (create dan))))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s)))
                   (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (define (zip-firsts xs ys)(cons (first xs) (first ys)))
  (define (sort-cycle-list xs)(append (rest xs) (list(first xs))))
  (lambda ()(cons (zip-firsts xs ys) (cycle-lists (sort-cycle-list xs) (sort-cycle-list ys)))))

(define (vector-assoc v vec)
  (define (find v vec pos)
       (if (=  (vector-length vec) pos)
          #f
          (if(pair? (vector-ref vec pos))
             (if (equal? (car (vector-ref vec pos)) v)
             (vector-ref vec pos)
             (find v vec (+ pos 1)))
         (find v vec (+ pos 1)))))
  (find v vec 0))

(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)] ; vector of cached results or #f
          [pos 0]
          [f (lambda (v)
               (let ([cached-v (vector-assoc v memo)])
                 (if cached-v 
                     cached-v
                     (let ([new-ans (assoc v xs)])
                       (if new-ans
                           (begin 
                         (vector-set! memo pos new-ans)
                         (if (= pos (- (vector-length memo) 1))
                             (set! pos 0)
                             (set! pos (+ pos 1)))
                         new-ans)
                           #f)))))])
    f))

  




  
  