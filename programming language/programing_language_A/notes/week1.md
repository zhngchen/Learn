## Standard ML

运行是自上而下的。

可以看作是有一个satic enviroment和 dymatic enviroment;

- 关注syntax; type checking; evaluation.

  比如：condition

  syntax: if e1 then e2 else e3

  type-checking: e1:bool;  e2, e3 相同的type 这样return 相同的type

  evauation: e1:true  得到e2对应的value v2; 否则v3.

- shadowing: 

  ```sml
  val a = 1
  val b = a
  val a = 2
  ```

  now : b = 1.(in the previous environment: b已经是1了，之后发生shadowing与b无关了)















## syntax

-要有两个operand，-4是不允许的， 可以用~ 4.

/只能用于float之间， 对于Int可以用 div







## repl

use  "xx.sml";一般只能用于开头哦，要在中途使用请重启。