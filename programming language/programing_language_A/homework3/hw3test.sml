use "other3.sml";
(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["a","B","C"] = ["B","C"]
val test1_2 = only_capitals ["a","bac","C"] = ["C"]


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","ac"] = "bc"
val test2_2 = longest_string1 [] = ""


val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A", "bc", "ac"] = "ac"
val test3_2 = longest_string2 [] = ""


val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["A","bc","ac"] = "bc"
val test4a_2 = longest_string3 [] = ""

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_1 = longest_string4 ["A", "bc", "ac"] = "ac"
val test4b_2 = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5_1 = longest_capitalized ["A","bca","CB"] = "CB"
val test5_2 = longest_capitalized ["AB","bca","CB"] = "AB"

val test6 = rev_string "abc" = "cba"
val test6_1 = rev_string "" = ""

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_1 = (first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5];false) 
                handle NoAnswer => true

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val test8_2 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (TupleP [Wildcard, Wildcard]) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable("abc")]) = 4

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x", TupleP [Variable("x"), Variable("x"), Wildcard]) = 2

val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP [Variable("x"), Variable("x")]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (Const(1), Variable("a")) = SOME [("a", Const(1))]

val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match (Const(1)) [UnitP, Variable("a")] = SOME [("a", Const(1))]

