(* 我写的一些有一些问题 ，或者答案比我好的都会列在这里*)
(*  我的，忽略了空的months  hw1已经改动以提交答案*)
fun number_in_months(date_lst: (int*int*int) list, months:int list) = 
    if null (tl months)
    then number_in_month(date_lst, hd months)
    else number_in_month(date_lst, hd months) + number_in_months(date_lst, tl months)
(* answer *)
fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* int * int list -> int *)
fun number_before_reaching_sum(sum: int, int_lst: int list) = 
    if hd int_lst >= sum
    then 0
    else 1 + number_before_reaching_sum(sum, (hd int_lst + hd (tl int_lst)) :: tl (tl int_lst))
    (* 将第一个元素加到剩下的list的第一个元素上 *)
(* 啊，太笨了 *)

fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)