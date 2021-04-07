#lang racket

; 我们不认为let是必要的 分行已经很清楚了
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]

; 我觉得自己写的也很不错
        [(mlet? e) 
         (let* ([v (eval-under-env (mlet-e e) env)]
                [newenv (cons (cons (mlet-var e) v) env)]) 
           (eval-under-env (mlet-body e) newenv))]
; 比我的要精炼
        [(call? e)
         (let ([cl  (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cl)
               (let* ([fn (closure-fun cl)]
                      [bodyenv (cons (cons (fun-formal fn) arg)
                                     (closure-env cl))]
                      [bodyenv (if (fun-nameopt fn)
                                   (cons (cons (fun-nameopt fn) cl)
                                         bodyenv)
                                   bodyenv)])
                 (eval-under-env (fun-body fn) bodyenv))
               (error "MUPL function call with nonfunction")))]


; 我犯了错误没有理解题目，是在写macros,应当生成语法
; hw5中已经在修改.
(define (ifaunit e1 e2 e3) (if (aunit? e1) e2 e3))

; 第四题 using the language 我们在写MUPL。使用变量 (var "x")