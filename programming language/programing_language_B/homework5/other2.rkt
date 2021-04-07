;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; part a
(define (racketlist->mupllist rl)
  (cond [(null? rl) (aunit)]
        [#t
         (apair (car rl) (racketlist->mupllist (cdr rl)))]))

;; part b
(define (mupllist->racketlist ml)
  (cond [(aunit? ml) null]
        [#t
         (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))]))
;; Problem 2
;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e) e]
        [(closure? e) e]
        [(aunit? e) (aunit)]
        [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)(int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater expression expected a number as first two args given otherwise")))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let([clo (eval-under-env (call-funexp e) env)]
              [arg (eval-under-env (call-actual e) env)])
           (if (closure? clo)
               (let ([env2  (cons (cons (fun-formal (closure-fun clo)) arg)
                                  (cons
                                   (cons (fun-nameopt(closure-fun clo)) clo)
                                   (closure-env clo)))])
                 (eval-under-env (fun-body (closure-fun clo)) env2))
               (error "you can't call what is not a function")))]
        [(mlet? e)
         (let* ([s1 (mlet-var e)]
                [v1 (eval-under-env (mlet-e e) env)]
                [p  (cons s1 v1)]
                [env2 (cons p env)])
           (eval-under-env (mlet-body e) env2))]
       [(apair? e)
        (apair (eval-under-env (apair-e1 e) env)
               (eval-under-env (apair-e2 e) env))]
       [(fst? e) (apair-e1 (eval-under-env (fst-e e) env))]
       [(snd? e) (apair-e2 (eval-under-env (snd-e e) env))]
       [(isaunit? e)
        (if (aunit? (eval-under-env (isaunit-e e) env))
            (int 1)
            (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "v1" e1
        (mlet "v2" e2
              (ifgreater (var "v1") (var "v2") e4 (ifgreater (var "v2")  (var "v1") e4 e3)))))
;; Problem 4

(define mupl-map (fun "f1" "fn"
                      (fun "f2" "lst"
                           (ifaunit (var "lst")
                                    (aunit)
                                    (apair (call (var "fn") (fst (var "lst")))
                                           (call (var "f2") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "f1" "i"
             (call (var "map") (fun "temp" "arg" (add (var "i") (var "arg"))) ))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
