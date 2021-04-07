(* 
Question 1: 
    Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
    the First argument is a date that comes before the second argument. (If the two dates are the same,
    the result is false.)
*)

fun is_equal (date1 : int * int * int, date2 : int * int * int) =
    (#1 date1 = #1 date2) andalso (#2 date1 = #2 date2) andalso (#3 date1 = #3 date2)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if is_equal (date1, date2)
    then false
    else (#1 date1 <= #1 date2) andalso (#2 date1 <= #2 date2) andalso (#3 date1 <= #3 date2)
val t1 =  is_older ((1,2,3),(4,5,6))							   
(* val test1_1 = is_older((1,2,3), (1,2,4)) = true
val test1_2 = is_older((1,2,3), (1,2,3)) = false
val test1_3 = is_older((4,2,3), (1,2,3)) = false
val test1_4 = is_older((3,4,5), (4,2,1)) = true *)
(*
Question 2:
    Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
    how many dates in the list are in the given month
*)

fun number_in_month (dates : (int * int * int) list, month : int) =
    (*iterate_months (dates, month, 0);*)
    if null dates
    then 0
    else if #2 (hd dates) = month
         then 1 + number_in_month (tl dates, month)
         else number_in_month (tl dates, month)
val t2 = number_in_month ([(2012,2,28),(2013,12,1)],4)
			     
(*
Question 3:
    Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
    and returns the number of dates in the list of dates that are in any of the months in the list of months.
    Assume the list of months has no number repeated. Hint: Use your answer to the previous problem
*)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)
val t3 =number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])							       


(*
Question 4:
    Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
    list holding the dates from the argument list of dates that are in the month. The returned list should
    contain dates in the order they were originally given.
*)

fun dates_in_month (dates : (int * int * int) list, month : int ) =
    if null dates
    then []
    else if #2 (hd dates) = month
        then hd dates :: dates_in_month (tl dates, month)
        else dates_in_month (tl dates, month)
val t4 = dates_in_month ([(2012,2,28),(2013,12,1)],4)
			    
(*
Question 5:
    Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
    and returns a list holding the dates from the argument list of dates that are in any of the months in
    the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
    previous problem and SML's list-append operator (@).
*)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

val t5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])
							     
(*
Question 6:
    Write a function get_nth that takes a list of strings and an int n and returns the n'th element of the
    list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
    your function may apply hd or tl to the empty list in this case, which is okay.
*)

fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)
val t6 = get_nth (["hey","you", "there", "stop",  "you"], 3)
		 
(*
Question 7:
    Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
    (for example). Use the operator ^ for concatenating strings and the library function Int.toString
    for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
    Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
    comma following the day and use capitalized English month names: January, February, March, April,
    May, June, July, August, September, October, November, December.
*)
val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
fun date_to_string (date : (int * int * int)) =
    get_nth (months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString (#1 date);
val t7 = date_to_string (2014, 7, 10) 

(*
Question 8:
    Write a function number_before_reaching_sum that takes an int called sum, which you can assume
    is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
    You should return an int n such that the First n elements of the list add to less than sum, but the First
    n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
    value; it is okay for an exception to occur if this is not the case.
*)
fun number_before_reaching_sum (sum : int, values : int list) =
    let
        fun iterate_sum (i : int, sum_stop : int, max : int, v : int list) =
            if sum_stop + hd v >= max
            then i - 1
            else iterate_sum (i + 1, sum_stop + hd v, max, tl v)
    in 
        iterate_sum (1, 0, sum, values)
    end;
val t8 = number_before_reaching_sum (10, [2,3,4,5,6])

(*
Question 9:
    Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
    what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
    answer to the previous problem.
*)

val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
fun what_month (day_of_year : int) =
    number_before_reaching_sum (day_of_year, month_days) + 1;
val t9 = what_month 60

(*
Question 10:
    Write a function month_range that takes two days of the year day1 and day2 and returns an int list
    [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
    of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else 
        let fun countUp (from : int, to : int) =
            if from = to
            then [to]
            else from :: countUp (from + 1, to)
        in
            countUp (what_month (day1), what_month (day2))
        end
val t10 = month_range (31, 34)
			 
(*
Question 11:
    Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
    evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
*)
fun oldest (dates : (int * int * int) list) =
    if null dates
        then NONE
    else
        let fun get_oldest (dates : (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else
                    let
                        val last = get_oldest (tl dates)
                        val first = hd dates
                    in
                        if is_older (first, last)
                        then first
                        else last
                    end
        in
            SOME (get_oldest dates)
        end
val t11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)])
