exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
(* (unit -> int) -> (string -> int) -> pattern -> int *)
(* 两个基本的映射：f1: Wildcard -> int; f2: Variable ->int. 统计一个pattern中wildcard和variable的情况。 *)
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals xs = 
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs = 
    List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" xs


fun longest_string2 xs = 
    List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" xs

(* f:int*int -> bool *)
fun longest_string_helper f xs = 
    case xs of
        [] => ""
      | x::other => let val y = longest_string_helper f other
                    in if f(String.size(x), String.size(y)) then x else y
                    end

val longest_string3 = longest_string_helper (fn (x, y) => x >= y)
val longest_string4 = longest_string_helper (fn (x, y) => x > y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

(* f: 'a -> 'b option; result: 'b *)
fun first_answer f xs = 
    case xs of
        [] => raise NoAnswer
      | x::other => case f(x) of
                        SOME v => v
                      | NONE => first_answer f other

(* f: 'a -> 'b list option; result : 'b list option *)
fun all_answers f xs = 
    case xs of
        [] => SOME []
      | x::other => case f x of
                        NONE => NONE
                      | SOME lst => let val tl = all_answers f other
                                    in if tl = NONE then NONE else SOME (lst @ (valOf tl))
                                    end


val count_wildcards = g (fn () => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size s)
fun count_some_var (s, p) = g (fn () => 0) (fn str => if s = str then 1 else 0) p

fun check_pat p = 
    let 
        fun get_vars p =
            case p of
                Wildcard => []
              | Variable s => [s]
              | TupleP ps => List.foldl (fn (p, lst) => (get_vars p) @ lst) [] ps
              | ConstructorP(_,p) => get_vars p
              | _ => []
        fun is_repeat lst =
            case lst of
                [] => false
              | x::other => (List.exists (fn y => y = x) other) orelse is_repeat(other)
    in
        Bool.not ((is_repeat o get_vars) p)
    end


fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const i1, ConstP i2) => if i1 = i2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps) 
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v, p) else NONE
      |(_, _) => NONE



fun first_match v ps = 
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE

