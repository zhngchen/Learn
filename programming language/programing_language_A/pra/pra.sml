(* datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

type name_record = { student_num : int option, 
                     first       : string, 
                     middle      : string option, 
                     last        : string }

(* 不写c:card 是不行的，type checker需要知道这个 *)
fun is_Queen_of_Spades (c:card) = 
    #1 c = Spade andalso #2 c = Queen

val c1 = (Diamond,Ace)
(* c2: type, 会让编译器检查 *)
val c2 : suit * rank = (Heart,Ace)
val c3 = (Spade,Ace) *)

(* val b = 3
fun f x =
    x + b
val b = 5
val z = f 4 *)

val b = ref 3
fun f x =
    x + (!b)
val _ = b:=5
val z = f 4