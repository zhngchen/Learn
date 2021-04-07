(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*  This is a function called all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. *)

fun all_except_option (examKey, stringList) = 
    let 
        fun helper (curStringList, pastStringList) = 
            case curStringList of 
                [] => NONE
		   | i::x' => if same_string(i, examKey)
                              then SOME (pastStringList @ x')
                              else helper (x', i::pastStringList)
    in  
        helper (stringList,[])
    end 



(* more concise answer *)
(*
fun all_except_option (s,xs) =
  case xs of
      [] => NONE
    | x::xs' => if same_string(s,x)
                then SOME xs'
                else case all_except_option(s,xs') of
                         NONE => NONE
                       | SOME y => SOME(x::y)
*)


(* Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list. The result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result. *)

fun get_substitutions1 (substitutions, examKey) = 
    case substitutions of 
        [] => []
	   | cur :: rem => case all_except_option (examKey, cur) of
			       NONE => get_substitutions1 (rem, examKey)
			    | SOME i => i @ get_substitutions1 (rem, examKey) 


(* Tail recursive version of get_substitutions1 *)

fun get_substitutions2 (substitutions, examKey) = 
    let fun aux (substitutions, acc) =
	    case substitutions of 
			 [] => acc
		       | x :: xs' => case all_except_option(examKey, x) of
					 NONE => aux (xs', acc)
					      | SOME i => aux(xs', i@acc) 
    in
	aux(substitutions, [])
    end

(* Write a function similar_names, which takes a string list list of substitutions and a full name of type and returns a list of full
names. *)

fun similar_names (substitutions, {first = first, middle = middle, last = last}) =
    let val nameList = get_substitutions2 (substitutions, first)
    in let fun acc (nameList, start) =
		case nameList of 
		    [] => [{first = first, last = last, middle = middle}]
		       | cur::rem => [{first = cur, last = last, middle = middle}]@acc(rem, start)
       in acc (nameList, [{first = first, last = last, middle = middle}])
       end
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). *)
fun card_color (suit, rank) =
    case suit of 
	Spades => Black
     | Clubs => Black 
     | Diamonds => Red
     | Hearts => Red  


(* Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). *)
fun card_value (suit, rank) =
    case rank of 
	Ace => 11
      | Num i => i 
     | _ => 10 


(* Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. *)


(**********************************************************************
NEED REVISE
**********************************************************************)
fun remove_card (cs, c, e) = 
    case cs of
	[] => raise e
     | cur :: rem  => if cur = c
		      then remove_card(rem, c, e)
		      else cur :: remove_card(rem, c, e)


(* Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color. *) 
fun all_same_color (cs) =
    case cs of
	[] => true
     | cur::rem => let val curColor = card_color(cur)
		   in  case rem of 
			   [] => true
			| x::x' => if curColor = card_color(x)
				   then all_same_color (rem)
				   else false
		   end


(* Write a function sum_cards, which takes a list of cards and returns the sum of their values. *)
fun sum_cards (cs) =
    let fun aux (stack, sum) = 
	    case stack of 
		[] => sum
	     | x::xs => aux(xs, sum + card_value(x))
    in aux(cs, 0)
    end


(*  Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described *)
fun score (cs, goal) = 
    let val sum = sum_cards(cs)
	val same = all_same_color(cs)
    in			   
	if sum > goal 
	then     if same
		     then (3*(sum - goal)) div 2
		     else 3*(sum - goal)
	else if same
	then (goal - sum) div 2
	else goal - sum
    end 


(* Write a function officiate, which ¡°runs a game.¡±  *)
fun officiate (cardList, moveList, goal) = 
    let fun continue_playing (moves, helds, card) =
	    if sum_cards(helds) > goal
	    then score(helds, goal)
	    else case moves of
		     [] => score(helds, goal)
		   | x::x' => case x of  
				  Discard c => continue_playing(moves, remove_card(helds, c, IllegalMove),card)
				| Draw => case card of 
						[] => score(helds, goal)
					      | y::y' => continue_playing(x', y::helds, y')
    in continue_playing(moveList, [], cardList)
    end


(* Challenge Problems *)
