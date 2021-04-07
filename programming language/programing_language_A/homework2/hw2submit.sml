fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, str_list) =
    case str_list of
       [] => NONE
     | a :: lst =>if same_string(str, a) then SOME lst 
                  else case all_except_option(str, lst) of
                        NONE => NONE
                      | SOME x => SOME (a::x)


fun get_substitutions1(substitutions, str) = 
    let fun get_strings(lst_option) = 
        case lst_option of
            NONE => []
          | SOME lst => lst
    in case substitutions of
       [] => []
     | lst :: other => get_strings(all_except_option(str, lst)) @ get_substitutions1(other, str)
    end


fun get_substitutions2(substitutions, str) = 
    let 
        fun get_strings(lst_option) = 
            case lst_option of
              NONE => []
            | SOME lst => lst
        fun aux(substitutions, str, acc) =
            case substitutions of
                [] => acc
            | lst :: other => aux(other, str, get_strings(all_except_option(str, lst)) @ acc)
    in
        aux(substitutions, str, [])
    end



fun similar_names(substitutions, full_name) = 
    let
        val {first=name1, middle=name2, last=name3} = full_name
        val first_name_lst = get_substitutions1(substitutions, name1)
        (* generate full name with first_name_lst; without origin full name *)
        fun help(name_list) =
            case name_list of
                [] => []
              | name :: other => {first=name, middle=name2, last=name3} :: help(other)
    in
        full_name :: help(first_name_lst)
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
fun card_color card = 
    case card of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black


fun card_value card = 
    case card of
        (_, Ace) => 11
      | (_, Num x) => x
      | _ => 10


fun remove_card(cards, card, IllegalMove) = 
    case cards of
        [] => raise IllegalMove
      | c :: other => if c = card then other else c :: remove_card(other, card, IllegalMove)


fun all_same_color cards  = 
    case cards of
        [] => true
      | _ :: [] => true
      | c1::c2::other => card_color(c1) = card_color(c2) andalso all_same_color(c2::other)


fun sum_cards cards =
    let 
        fun aux(cards, acc) =
            case cards of
                [] => acc
              | c::other => aux(other, acc+card_value(c))
    in 
        aux(cards, 0)
    end


fun score(held_cards, goal) =
    let 
        val sum = sum_cards(held_cards)
        val pre_score = if sum < goal then goal - sum else 3 * (sum-goal)
    in
        if all_same_color(held_cards) then pre_score div 2 else pre_score
    end

fun officiate(cards, moves, goal) = 
    let 
        fun state(cards, moves, held_cards, goal) =
            case moves of
                [] => score(held_cards, goal)
                (* 当前的操作为Draw, 就要根据cards的情况分类 *)
              | Draw::moves' => (case cards of 
                                    [] => score(held_cards, goal)
                                  | c::cards' => if sum_cards(c::held_cards) > goal 
                                                then score(c::held_cards, goal)
                                                else state(cards',moves',c::held_cards,goal))
              | (Discard c)::moves' => state(cards,moves',remove_card(held_cards,c,IllegalMove),goal)
    in
        state(cards, moves, [], goal)
    end


(* my test *)
(* val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_1 = all_except_option("a", ["b", "c", "d"]) = NONE
val test1_2 = all_except_option("a", ["b", "a", "c"]) = SOME ["b", "c"]

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
val test5_1 = card_color(Hearts, Ace) = Red
val test5_2 = card_color(Spades, King) = Black

val test6 = card_value (Clubs, Num 2) = 2
val test6_1 = card_value (Clubs, Ace) = 11
val test6_2 = card_value (Hearts, Jack) = 10


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = remove_card ([(Hearts, Ace), (Spades, King)], (Hearts, Ace), IllegalMove) = [(Spades, King)]
(* val test7_2 = remove_card ([(Hearts, Ace), (Spades, King)], (Hearts, Num 3), IllegalMove) =  *)
(* 怎么检测有exception 发生呢 *)

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [(Hearts, Ace), (Spades, Ace)] = false
val test8_2 = all_same_color [] = true
val test8_3 = all_same_color [(Hearts, Jack)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_1 = sum_cards [(Clubs, King),(Hearts, Num 6)] = 16
val test9_2 = sum_cards [(Clubs, Ace),(Hearts, Num 9)] = 20
val test9_3 = sum_cards [(Clubs, Ace),(Hearts, Num 8),(Spades, Queen)] = 29

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_1 = score ([(Hearts, Num 6),(Clubs, Num 4)],10) = 0
val test10_2 = score ([(Hearts, King),(Clubs, Num 4)],10) = 12
val test10_3 = score ([(Hearts, King),(Diamonds, Num 4)],10) = 6

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test11_1 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test11_2 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) *)