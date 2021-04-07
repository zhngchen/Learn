(* 记录pattern matching的用法：匹配类型，获取value。对于one of, each of 皆可*)
(* _匹配任意，但不想使用值 *)

datatype exp = Constant of int 
             | Negate of exp 
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
      | Negate e2  => ~ (eval e2)
      | Add(e1,e2) => (eval e1) + (eval e2)
      | Multiply(e1,e2) => (eval e1) * (eval e2)

fun number_of_adds e =
    case e of
        Constant i      => 0
      | Negate e2       => number_of_adds e2
      | Add(e1,e2)      => 1 + number_of_adds e1 + number_of_adds e2
      | Multiply(e1,e2) => number_of_adds e1 + number_of_adds e2

val example_exp = Add (Constant 19, Negate (Constant 4))

val example_ans = eval example_exp



(* 对于each of*)

(* 要注意 'a list 才是type *)
datatype 'a mylist = Empty | Cons of 'a * 'a mylist

fun append (xs,ys) =
    case xs of
        [] => ys
      | x::xs' => x :: append(xs',ys)

(* function 也是pattern matching *)
fun sum_triple1 (triple : int * int * int) =
    case triple of
      (x,y,z) => z + y + x
fun sum_triple3 (x,y,z) =
    x + y + z


(* 对于nested patterns 可以对多个List进行匹配*)
fun zip3 list_triple =
    case list_triple of 
	([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
      | _ => raise ListLengthMismatch

(* and the inverse *)
fun unzip3 lst =
    case lst of
	[] => ([],[],[])
      | (a,b,c)::tl => let val (l1,l2,l3) = unzip3 tl
		       in
			   (a::l1,b::l2,c::l3)
		       end

 fun nondecreasing xs =
    case xs of
	[] => true
      | x::[] => true
      | head::(neck::rest) => (head <= neck andalso nondecreasing (neck::rest))

