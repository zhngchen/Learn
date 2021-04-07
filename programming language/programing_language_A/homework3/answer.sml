(* 前几个问题根本没必要写fun, val即可 *)

(* 可以参考变量命名。 *)
val longest_string1 = 
    List.foldl (fn (s,sofar) => if String.size s > String.size sofar
				                        then s
				                        else sofar) 
(* 明明就是很容易啊。接受的函数只要比较两个string就好了。 *)
fun longest_string_helper f = 
    List.foldl (fn (s,sofar) => if f(String.size s,String.size sofar)
				                        then s
				                        else sofar) 
	                              ""
val longest_string3 = longest_string_helper (fn (x,y) => x > y) 
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* 利用acc更好地集中要选中的内容。 *)
fun all_answers f xs =
    let fun loop (acc,xs) =
        case xs of
		        [] => SOME acc
	        | x::xs' => case f x of 
                          NONE => NONE
              			    | SOME y => loop((y @ acc), xs')
    in loop ([],xs) end
(* 基本思路相同， *)
fun check_pat pat =
    let fun get_vars pat =
          case pat of
              Variable s => [s]
            | TupleP ps => List.foldl (fn (p,vs) => get_vars p @ vs) [] ps
            | ConstructorP(_,p) => get_vars p
            | _ => []
        fun unique xs =
          case xs of
              [] => true
            | x::xs' => (not (List.exists (fn y => y=x) xs'))
                        andalso unique xs'
    in
        unique (get_vars pat)
    end