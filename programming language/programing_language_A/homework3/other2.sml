(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

(* Problems 1-8 *)

fun only_capitals (str_ls : string list) = 
	List.filter 
		(fn str => Char.isUpper (String.sub (str, 0)))
		str_ls
	

fun longest_string1 (str_ls : string list) =
	List.foldl
		(fn (s1, s2) => case String.size s1 > String.size s2 of
								true => s1
							| 	false => s2)
		""
		str_ls


fun longest_string2 (str_ls : string list) = 
	List.foldl
		(fn (s1, s2) => case String.size s1 >= String.size s2 of
								true => s1
							| 	false => s2)
		""
		str_ls


fun longest_string_helper compare_fn xs =  (* since we are currying, we can pass multiple args *)
	List.foldl
		(fn (s1, s2) => if compare_fn (String.size s1, String.size s2)
						then s1
						else s2
		)
		""
		xs


val longest_string3 =
	longest_string_helper (fn (x, y) => x > y) 
	(* partial application so that string list can be passed later with currying *)
								

val longest_string4 =
	longest_string_helper (fn (x, y) => x >= y)
							

val longest_capitalized = longest_string1 o only_capitals 


fun rev_string (str) =
	(String.implode o rev o String.explode) str


fun first_answer check_fn xs =
	case xs of
		[] => raise NoAnswer
	| 	hd::tl => 	(case check_fn hd of
							NONE => first_answer check_fn tl
						|   SOME(v) => v
					)


fun all_answers check_fn xs =
	let 
		fun helper (acc, lst) = 
			case lst of
				[] => SOME(acc)
				|	hd::tl => 	( case check_fn hd of
										NONE => NONE
									|	SOME(v) => helper(v @ acc, tl)
								)
	in
		helper ( [], xs)
	end


(* provided datatypes & function for further problems *)
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

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps  (* i is the accumulator *)
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* problem 9 onwards *)

fun count_wildcards (ptn) =
	g (fn () => 1) (fn x => 0) ptn

fun count_wild_and_variable_lengths ptn =
	g (fn () => 1) (fn x => String.size x) ptn

fun count_some_var (str, ptn) =
	g (fn () => 0) (fn x => if x = str then 1 else 0) ptn


fun check_pat ptn =
	let 
		fun get_variable_strings p = 
			case p of 
				Variable x => x::[]
			|	TupleP ps => List.foldl (fn (p, acc) => acc @ get_variable_strings (p) ) [] ps
			|	_ => []
		fun has_duplicates str_ls =
			case str_ls of 
				[] => false
			|	hd::[] => false
			|	hd::tl => (
							if List.exists (fn x => x = hd) tl
						  	then true
						  	else has_duplicates tl
						  )			
	in 
		not ((has_duplicates o get_variable_strings) ptn)
	end


fun match (v, p) =
	case (v,p) of
		(_, Wildcard) => SOME []
	|	(v, Variable s) => SOME [(s, v)]
	|	(Unit, UnitP) => SOME []
	|	(Const i, ConstP j) => if i=j then SOME [] else NONE
	|	(Tuple vs, TupleP ps) => 	if List.length vs = List.length ps
									then all_answers match (ListPair.zip (vs, ps))
									else NONE
	|	(Constructor(s2, v'), ConstructorP(s1, p')) => 	if s1 = s2 
														then match (v', p')
												   		else NONE 
	|	_ => NONE
	

fun first_match v ps = 
	SOME (first_answer (fn p => match (v, p)) ps)
	handle NoAnswer => NONE

(**** for the challenge problem only ****)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
