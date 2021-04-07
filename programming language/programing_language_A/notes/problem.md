- binding  pattern matching 要好好看。

- 关于type check: 什么时候要写 type?

  fun sum (x, y, z) = x + y + z; 可不写。

  fun          c : card  = #1 c           要写。

  

  fun mystery x = case x of

  (1,b,c) => #2 x + 10

  | (a,b,c) => a * c   

  能判断出：`int*int*int -> int`
  
  已解决，与type inferce有关，# 1 多个元素
  
- 再理解tail recursion,  tail position.

- 再想dymatic scope  

  let expression 当中的binding,  是当前的binding吗？

- fold 那一节，要再看。