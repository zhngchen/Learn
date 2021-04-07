(* Coursera Programming Languages, Homework 3, Provided Code *)

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
(* 1. Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
val only_capitals =
    List.filter(fn list => (Char.isUpper o String.sub)(list, 0))

(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)
val longest_string1 = 
    List.foldl(fn (x,xs) => if  (String.size x) > (String.size xs)
			    then x 
			    else xs)""

(* 3.Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Still use foldl and String.size. *)
val longest_string2 =
    List.foldl(fn (x,xs) => if (String.size x) < (String.size xs)
			    then xs 
			    else x)""


(* 4.Write functions longest_string_helper, longest_string3, and longest_string4 *)
fun longest_string_helper f = List.foldl(fn(x,xs) => 
				  if f(String.size x, String.size xs)
				  then x
				  else xs)""


val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)


(* 5.Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. *)
fun longest_capitalized x = (longest_string3 o only_capitals) x 

(* 6. Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. *)
fun rev_string x = (String.implode o List.rev o String.explode) x

(* 7. Write a function first_answer. The first argument should be applied to elements of the second argument in order until the first time it returns SOME v for some v and then v is the result of the call to first_answer.If the first argument returns NONE for all list elements, then first_answer should raise the exception NoAnswer. *)

fun first_answer f xs = case xs of 
			    [] => raise NoAnswer
			  | x::x' => case f x of 
					 SOME x => x  		
				       | NONE => first_answer f x' 

(* 8. Write a function all_answers. The first argument should be applied to elements of the second argument. The calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of all_answers i SOME lst where lst is lst1, lst2, ..., lstn appended together. *)
fun all_answers f lst =
    let fun helper xs pre =
	    case (xs, pre) of 
		([],_) => pre  
	      | (x::x',SOME s) =>(case f x of 
				 NONE => NONE
			       | SOME y => helper x' (SOME(y@s))) 
	      | _ => NONE 
    in
	helper lst (SOME[])
    end 

(* 9a. Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard patterns it contains. *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9b. Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains. *)
val count_wild_and_variable_lengths = g (fn _ =>1)(fn x => String.size x)

(* 9c.  Use g to define a function count_some_var that takes a string and a pattern (as a pair) and returns the number of times the string appears as a variable in the pattern. *)
fun count_some_var (str,v) = g (fn _=>0) (fn x => if String.isSubstring str x
						then 1
						else 0) v

(* 10.  Write a function check_pat that takes a pattern and returns true if and only if all the variables appearing in the pattern are distinct from each other (i.e., use different strings). *)
fun check_pat p =
    let fun toList pat pre= case pat of 
				Variable x => x::pre
			       | ConstructorP (_,p) => toList p pre
			       | TupleP ps => List.foldl (fn (p,pre) => (toList p [])@pre) [] ps
			       | _ =>[] 
    in
	let 
	    val list = toList p []
	    fun check lst = 
		case lst of 
		    [] => true
		 | x :: xs => if List.exists (fn item => item = x) xs
			      then false 
			      else true 
	in check list
	end 
    end 


(* 11. Write a function match that takes a valu * pattern and returns a (string * valu) list option, namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does. *)
fun match (v,p) = 
    case p of 
	Wildcard => SOME []
      | UnitP  => (case v of Unit => SOME []
			   | _ => NONE) 
      | Variable x => SOME[(x,v)]
      | ConstP i =>(case v of 
			Const j => if i = j 
				   then SOME[] 
				   else NONE
		      | _ => NONE) 
      | TupleP ps => (case v of 
			  Tuple vs => if List.length(ps) = List.length(vs)
				      then all_answers match (ListPair.zip(vs, ps))
				      else NONE
			| _ => NONE)
      | ConstructorP (s1,pt) => (case v of 
				    Constructor (s2,vt) => if s1=s2
							  then match (vt,pt)
							  else NONE
				  | _ => NONE)


(* 12. Write a function first_match that takes a value and a list of patterns and returns a (string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where lst is the list of bindings for the first pattern in the list that matches. *)
fun first_match v pat = 
    SOME(first_answer (fn p => match(v,p)) pat) handle NoAnswer => NONE


(* Chanllenge Problems *)
