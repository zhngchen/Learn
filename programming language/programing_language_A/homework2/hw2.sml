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

(* 你看，在state里，goal是没有必要的 *)
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


(* for challenge problems *)
(* 先统计有几张A， 我们知道A都是11情况下的sum计算， 如果有一张A算作了1， 比sum少10而已。 要做的也即是让pre_score最小 *)
(* 思路：统计A的数目，逐个比较几张A算作1下的最佳情况 *)
(* logic: getting the number of Ace, if one Ace counts 1, then sum turns into sum - 10*)
fun get_ace_num(cards) = 
    case cards of
        [] => 0
      | (_, Ace)::rest => 1 + get_ace_num(rest)
      | _::rest => get_ace_num(rest)


fun score_challenge(held_cards, goal) = 
    let
        val a_num = get_ace_num(held_cards)
        val pre_sum = sum_cards(held_cards)

        fun score(sum, goal) = 
            let
                val pre_score = if sum <= goal
                                then goal - sum
                                else 3 * (sum - goal)
            in
                if all_same_color(held_cards)
                then pre_score div 2
                else pre_score
            end

        fun best_score(a_num, sum, goal) =
            if a_num = 0 orelse sum <= goal
            then score(sum, goal)
            else let 
                    val score1 = score(sum, goal)
                    val score2 = best_score(a_num - 1, sum - 10, goal)
                 in
                    Int.min(score1, score2)
                end
    in
        best_score(a_num, pre_sum, goal)
    end



(* 思路也很简单，只要把条件换成goal小于把A都当作1的分数即可 *)
(* condition: goal < sum(for all Aces count 1) *)
(* FIXME 虽然提交了，但是还是有问题的 *)
fun officiate_challenge(cards, moves, goal) = 
    let 
        fun state(cards, moves, held_cards, goal) =
            case moves of
                [] => score(held_cards, goal)
              | Draw::moves' => (case cards of 
                                    [] => score(held_cards, goal)
                                  | c::cards' => if sum_cards(c::held_cards) - 10 * get_ace_num(c::held_cards) > goal 
                                                then score(c::held_cards, goal)
                                                else state(cards',moves',c::held_cards,goal))
              | (Discard c)::moves' => state(cards,moves',remove_card(held_cards,c,IllegalMove),goal)
    in
        state(cards, moves, [], goal)
    end