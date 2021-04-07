(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* 感觉他做的还不错 *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*fun all_except_option (str, strlist) =
    case strlist of
	[] => None
		  (x:xs) => same_string(x,str) orelse strlist(xs)*)

fun all_except_option (str, strlist) =
    case strlist of
	[] => NONE
      | (first :: rest) => if same_string(str, first) then SOME rest else
			   case all_except_option (str, rest) of
			       NONE => NONE 
			     | SOME strs => SOME(first::strs)

fun  get_substitutions1(strlistlist, str) =
     case strlistlist of
	 [] => [] 
       | first::rest  => (case all_except_option(str,first) of
			     NONE => []
			   | SOME strs => strs) @ get_substitutions1(rest, str)
					 
fun  get_substitutions2(strlistlist, str) =
     let fun listdel (strlist) =
	     case all_except_option(str, strlist) of
		 NONE => []
	       | SOME strs  => strs
	 fun aux(strlistlist, acc) =
	     case strlistlist of
		 [] => acc
	       | first::rest => aux(rest, acc@(listdel first))
     in
	 aux(strlistlist, [])
     end


fun similar_names(strlistlist, {first=x, middle=y, last=z}) =
    let val firstnames = x::(get_substitutions2(strlistlist,x))
	fun namesub (names) =
	    case names of
		[] => []
	      | aname::othernames => {first=aname, middle=y, last=z}::namesub(othernames)
    in
	namesub(firstnames)
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
fun card_color(x) =
    case x of
	(Spades,_) => Black 
     | (Clubs,_)  => Black 
     | _  => Red  
		
fun card_value(x) =
    case x of
	(_,Ace) => 11
     | (_,Num(i)) => i
     | _ => 10  

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
      | c'::cs' => if c' = c then cs' else
		   c'::remove_card(cs',c,e)

fun all_same_color [] = true
  | all_same_color (c::[]) = true
  | all_same_color (c'::(c::cs)) = (card_color(c')=card_color(c)) andalso all_same_color(c::cs)

fun sum_cards cs =
    let fun  aux(cs, acc) =
	case cs of
	    [] => acc
	  | c::cs' => aux(cs',acc + card_value(c))
    in
	aux(cs, 0)
    end

fun score (cs,goal) =
    let val handscore = sum_cards(cs)
	val colorbonus = if all_same_color(cs) then 2 else 1
    in
	if handscore >= goal then 3*(handscore - goal) div colorbonus
	else (goal - handscore) div colorbonus
    end

fun officiate (cards, moves, goal) =
    let fun hand(cards, moves, lasthand) =
	    if sum_cards(lasthand)>goal orelse moves = [] orelse (moves = [Draw] andalso cards=[])
	    then score(lasthand, goal)
	    else case (cards, moves) of
		     (c::cs, Draw::moves')=> hand(cs, moves', c::lasthand)
		   | (_,Discard(x)::moves') => hand(cards, moves', remove_card(lasthand,x,IllegalMove))
    in
	hand(cards, moves, [])
    end

(*Used for score_challenge and officiate_challenge *)
fun ace_count(cards) =
    case cards of
	[] => 0
     |  (_,Ace)::cards' => 1+ace_count(cards')
     |  _::cards' =>  ace_count(cards')
	
(*Logic: compare to [goal + (# of aces)*10, ..., goal+10, goal] and find minimum *)
fun score_challenge(cards,goal) =
    let fun goallist(acesleft)=
	    if acesleft = 0 then [goal]
	    else (goal + 10*acesleft)::goallist(acesleft-1)
	fun minovergoals(goals) =
	    case goals of
		x::[] =>score(cards,x)
	      | x::y::z => let val tl_ans = minovergoals(y::z) in
			       if score(cards,x)< tl_ans
			       then score(cards,x) else tl_ans
			   end			       
    in
	minovergoals(goallist(ace_count(cards)))
    end

(*Same as officiate except score -> score_challenge and
 in stopping condition goal -> goal+10*acenum *)
fun officiate_challenge (cards, moves, goal) =
    let
	fun hand(cards, moves, lasthand) =
	    let val acenum = ace_count(lasthand)
	    in
		if sum_cards(lasthand)>(goal+10*acenum) orelse moves = [] orelse (moves = [Draw] andalso cards=[])
		then score_challenge(lasthand, goal)
		else case (cards, moves) of
			 (c::cs, Draw::moves')=> hand(cs, moves', c::lasthand)
		       | (_,Discard(x)::moves') => hand(cards, moves', remove_card(lasthand,x,IllegalMove))
	    end
    in
	hand(cards, moves, [])
    end

(*Could be made more optimal  by changing cheatdiscard to check
 if discard+draw increases current score while still remaining <goal*)
fun careful_player (cards, goal) =
    let fun cheatdiscard(c,checked,tocheck) =
	    case tocheck of
		[] => NONE
	      | x::tocheck' => if score(c::(checked@tocheck'), goal) = 0 then SOME x else
			       cheatdiscard(c,x::checked,tocheck')
	fun state (cards, hand, goal) =
	    case cards of
		[] => []
	     |  c::cards' => if  score(hand, goal)=0 then state([], hand, goal)
			     else if sum_cards(c::hand) <= goal (*also catches sum_cards(hand)<goal-10*)
			     then Draw::state(cards', c::hand, goal)
			     else case cheatdiscard(c,[],hand) of
				      SOME x => Discard(x)::(Draw::state(cards', c::hand, goal))
				    | NONE  => state([], hand, goal)
    in
	state(cards,[],goal)
    end	
