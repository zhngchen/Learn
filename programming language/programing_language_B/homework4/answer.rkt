#lang racket

; 第一题只要两个情况就好。
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


; #5  我是怎样处理下一个元素。然而可以考虑当前元素的处理。
(define funny-number-stream 
  (letrec ([f (lambda (n) (cons (if (= (remainder n 5) 0) (- n) n)
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

; #6 其余写法。
(define (dan-then-dog)
  (cons "dan.jpg"
        (lambda () (cons "dog.jpg" dan-then-dog))))

(define dan-then-dog
  (letrec ([f (lambda (b) 
		            (if b
		                (cons "dan.jpg" (lambda () (f #f)))
		                (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))

; #9 只evaluate了一次vector-ref; and的使用. 多个条件当然要考虑逻辑量词了。
(define (vector-assoc v vec)
  (letrec ([loop (lambda (i)
                   (if (= i (vector-length vec))
                       #f
                       (let ([x (vector-ref vec i)])
                         (if (and (cons? x) (equal? (car x) v))
                             x
                             (loop (+ i 1))))))])
    (loop 0)))

; 有些检测可以在外部。
(define (vector-assoc v vec)
    (let ([n (vector-length vec)])
        (letrec ([aux (lambda (m) (cond [(= n m) n]
                                        [(and (pair? (vector-ref vec m)) (equal? (car (vector-ref vec m)) v)) m]
                                        [#t (aux (+ m 1))]))]
                 [index (aux 0)])
                 (if (= index n) #f (vector-ref vec index)))))


; #10  or and 是If的syntaxic sugar
(define (cached-assoc lst n)
  (let ([cache (make-vector n #f)]
        [next-to-replace 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v lst)])
            (and ans
                 (begin (vector-set! cache next-to-replace ans)
                        (set! next-to-replace 
                              (if (= (+ next-to-replace 1) n)
                                  0
                                  (+ next-to-replace 1)))
                        ans))))))))

; challenge
(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
      (let ([z x])
        (letrec ([loop (lambda ()
			                  (let ([w y])
		 	                    (if (or (not (number? w)) (>= w z))
			                        #t
			                        (loop))))])
          (loop))))))