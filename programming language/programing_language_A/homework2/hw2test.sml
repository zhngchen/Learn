use "hw2.sml";
(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_1 = all_except_option("a", ["b", "c", "d"]) = NONE
val test1_2 = all_except_option("a", ["b", "a", "c"]) = SOME ["b", "c"]


val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = 
             ["Fredrick","Freddie","F"]

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
(* val test7_2 = remove_card ([(Hearts, Ace), (Spades, King)], (Hearts, Num 3), IllegalMove);false handle IllegalMove => true  *)
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
              handle IllegalMove => true)


val test12 = score_challenge([(Hearts, Ace), (Clubs, Num 4)], 6) = 1
val test12_1 = score_challenge([(Hearts, Ace), (Clubs, Num 4)], 14) = 3
val test12_2 = score_challenge([(Hearts, Ace),(Clubs, Ace),(Spades,Ace)], 13) = 0





             
             
