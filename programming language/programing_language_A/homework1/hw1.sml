(* (int * int * int) * (int * int * int) -> bool *)
fun is_older(date1: int*int*int, date2: int*int*int) = 
    let 
        val y1 = #1 date1
        val y2 = #1 date2
        val m1 = #2 date1
        val m2 = #2 date2
        val d1 = #3 date1
        val d2 = #3 date2
    in 
        if y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
        then true
        else false
    end 


(* (int * int * int) list * int -> int *)
fun number_in_month(date_lst: (int*int*int) list, month:int) = 
    if null date_lst
    then 0
    else if (#2 (hd date_lst)) = month
    then 1 + number_in_month(tl date_lst, month) 
    else 0 + number_in_month(tl date_lst, month)

(* (int * int * int) list * int list -> int *)
fun number_in_months(date_lst: (int*int*int) list, months:int list) = 
    if null months
    then 0
    else number_in_month(date_lst, hd months) + number_in_months(date_lst, tl months)

(* (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month(date_lst: (int*int*int) list, month: int) = 
    if null date_lst
    then []
    else if (#2 (hd date_lst)) = month
    then hd date_lst :: dates_in_month(tl date_lst, month)
    else dates_in_month(tl date_lst, month)

(* (int * int * int) list * int list -> (int * int * int) list     *)
fun dates_in_months(date_lst: (int*int*int) list, months: int list) = 
    if null months
    then []
    else dates_in_month(date_lst, hd months) @ dates_in_months(date_lst, tl months)

(* string list * int -> string *)
fun get_nth(strings: string list, n: int) = 
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)

(* int * int * int -> string *)
fun date_to_string(date: int*int*int) = 
    let 
        val year = Int.toString(#1 date)
        val day = Int.toString(#3 date)
        val month_lst = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "Novermber", "December"]
        val month = get_nth(month_lst, #2 date)
    in
        month ^ " " ^ day ^ ", " ^ year
    end

(* int * int list -> int *)
fun number_before_reaching_sum(sum: int, int_lst: int list) = 
    if hd int_lst >= sum
    then 0
    else 1 + number_before_reaching_sum(sum, (hd int_lst + hd (tl int_lst)) :: tl (tl int_lst))
    (* 将第一个元素加到剩下的list的第一个元素上 *)

(* int -> int *)
fun what_month(days: int) = 
    let  
        val days_lst = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        number_before_reaching_sum(days, days_lst) + 1
    end

(* int * int -> int list *)
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(* (int * int * int) list -> (int * int * int) option *)
fun oldest(date_lst: (int*int*int) list) = 
    if null date_lst   (* empty list *)
    then NONE
    else if null (tl date_lst)  (* one element list *)
    then SOME (hd date_lst)
    else 
        let val tl_ans = oldest(tl date_lst)
        in 
            if is_older(hd date_lst, valOf tl_ans)
            then SOME (hd date_lst)
            else tl_ans
        end



(* for challenge problems *)
(* int * (int list) -> bool *)
fun month_in_list(month:int, months:int list) = 
    if null months
    then false
    else (month = hd months) orelse month_in_list(month, tl months)

(* int list -> int list *)
fun norepeat_months(months) = 
    if null months
    then []
    else if month_in_list(hd months, tl months)
    then norepeat_months(tl months)
    else (hd months) :: norepeat_months(tl months)
    

(* (int * int * int) list * int list -> int *)
fun number_in_months_challenge(date_lst: (int*int*int) list, months:int list) = 
   let val new_months = norepeat_months(months)
   in number_in_months(date_lst, new_months)
   end


(* !!  the sequence of the result may be not right*)
(* (int * int * int) list * int list -> (int * int * int) list     *)
fun dates_in_months_challenge(date_lst: (int*int*int) list, months: int list) = 
    let val new_months = norepeat_months(months)
    in dates_in_months(date_lst, new_months)
    end

(* int*int*int -> bool *)
fun reasonable_date(date: int*int*int) = 
    let 
        val y = #1 date
        val m = #2 date
        val d = #3 date
        val ordinary_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val leap_months = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun isleap(y:int) = 
            if (y mod 400 = 0) orelse (y mod 4 = 0 andalso y mod 100 <> 0)
            then true
            else false
        fun get_days(days_lst: int list, month) = 
            if month = 1
            then hd days_lst
            else get_days(tl days_lst, month - 1)
    in 
        if isleap(y)
        then (y > 0) andalso (m >= 1 andalso m <= 12) andalso (d <= get_days(leap_months, m))
        else (y > 0) andalso (m >= 1 andalso m <= 12) andalso (d <= get_days(ordinary_months, m))
    end
    
    