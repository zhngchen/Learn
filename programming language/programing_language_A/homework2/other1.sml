(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* all_except_option - takes a string and a string list.  
Returns NONE if the string is not in the list, 
else returns SOME lst where lst is identical to the argument list except the string is not in it.  
You may assume the string is in the list at most once.  
Use same_string, provided to you to compare strings.  *)
fun all_except_option(str, str_list) =
    let
	fun aux(s, tail, head) =
	    case tail of
		[] => NONE
	      | x::xs' => if x=s then SOME (head @ xs') else aux(s, xs', head @ [x])
    in
	aux(str, str_list, [])
    end

(* get_substitutions1 - takes a 'string list list' called "substitutions" and a string "s", returns a string list
 *)
fun get_substitutions1(substitutions : string list list, s : string) =
    case substitutions of
	[] => []
      | head::tail => case all_except_option(s, head) of
			  NONE => get_substitutions1(tail, s)
			| SOME x => x @ get_substitutions1(tail, s)

(* get_substitutions2 - same as ..1 but using tail-recursive local helper function *)
fun get_substitutions2(substitutions : string list list, s : string) =
    let
	fun aux(subs, str, answer) =
	    case subs of
		[] => answer
	      | head::tail => case all_except_option(str, head) of
				  NONE => aux(tail, str, answer)
				| SOME x => aux(tail, str, answer @ x)
    in
	aux(substitutions, s, [])
    end

(* similar_names - takes 'string list list' calles "substitutions" and 'full name' of type {first:string,middle:string,last:string} and returns a list of 'full name' with all first names substituted *)

fun similar_names(substitutions : string list list, full_name : {first:string,middle:string,last:string}) =
    let
	fun helper(subs, full_name : {first:string,middle:string,last:string} , list) =
	    case subs of
		[] => []
	      | head::tail => list @ [{first=head,
				       middle=(#middle full_name),
				       last=(#last full_name)}]
			      @ helper(tail, full_name, list)
    in
	[full_name] @ helper(get_substitutions1(substitutions, #first full_name),
			     full_name,
			     [])
    end
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* card_color - takes a card and returns its color *)
fun card_color(c : card) =
    case c of
	(Diamonds, _) => Red
      | (Hearts, _) => Red
      | _ => Black

(* card_value - takes a card and returns its value *)
fun card_value(c : card) =
    case c of
	(_, Ace) => 11
      | (_, King) => 10
      | (_, Queen) => 10
      | (_, Jack) => 10
      | (_, Num x) => x

(* remove_card - takes list of cards (cs), a card (c) and an exception (e).
Returns list without a card (first instance). If card is not in the list raise exception.
 *)
fun remove_card(cs : card list, c : card, e) =
    case all_except_option(c, cs) of
	NONE => raise e
      | SOME x => x

(* all_same_color - takes a list of cards and returns true if all the cards in the list are the same color *)
fun all_same_color(cs : card list) =
    case cs of
	[] => true
      | x::[] => true
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso
			      all_same_color(neck::rest))

(* sum_cards - takes a list of cards and returns the sum of their values *)
fun sum_cards(cs : card list) =
    let
	fun aux(cs, sum) =
	    case cs of
		[] => sum
	      | x::xs' => aux(xs', sum+(card_value x))
    in
	aux(cs, 0)
    end

(* score - takes a card list (cs) and int (goal) and computes the score *)
fun score(cs : card list, goal : int) =
    let val points = sum_cards cs
    in
	if points > goal then
	    if all_same_color cs then
		3 * (points - goal) div 2
	    else
		3 * (points - goal)
	else
	    if all_same_color cs then
		(goal - points) div 2
	    else
		goal - points
    end
(* officiate - takes card list (cs), move list (ms) and an int (goal) and returns a score at the end of the game *)
fun officiate(cs : card list, ms : move list, goal : int) =
    let
	fun helper(deck : card list, moves : move list, goal : int,
		   cards : card list) =
	    case moves of
		[] => score(cards, goal)
	      | head::tail => case head of
				  Discard c => helper(deck,
						      tail,
						      goal,
						      remove_card(cards, c, IllegalMove))
				| Draw => case deck of
					      [] => score(cards, goal)
					    | dhead::dtail => if card_value(dhead) +
								 sum_cards(cards) > goal
							      then score(dhead::cards, goal)
						else
						    helper(dtail, tail, goal, dhead::cards)
    in
	helper(cs, ms, goal, [])
    end
	
