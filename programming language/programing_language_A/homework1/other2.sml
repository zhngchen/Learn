(* Programming Languages, Part A *)
(* Homework 1 *)

fun is_older(x:int*int*int, y:int*int*int) =
  let 
    val y1 = #1 x
    val y2 = #1 y
    val m1 = #2 x
    val m2 = #2 y
    val d1 = #3 x
    val d2 = #3 y
  in
    y1 < y2 orelse y1 = y2 andalso m1 < m2 orelse m1 = m2 andalso d1 < d2
  end


fun number_in_month(dates:(int*int*int) list, month:int) = 
  let fun in_month(date:int*int*int) = 
        if #2 date = month 
        then 1 
        else 0 
  in 
    if null dates
    then 0
    else in_month(hd dates) + number_in_month(tl dates, month)
  end


fun number_in_months(dates:(int*int*int) list, months:int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates:(int*int*int) list, month:int) =
    if null dates 
    then []
    else if #2 (hd dates) = month
         then hd dates :: dates_in_month(tl dates, month)
         else dates_in_month(tl dates, month)


fun dates_in_months(dates:(int*int*int) list, months:int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(strs:string list, n:int) = 
  if n = 1
  then hd strs
  else get_nth(tl strs, n-1)


fun date_to_string(date:int*int*int) =
  let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
    get_nth(months,#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
  end

fun number_before_reaching_sum(sum:int, ns:int list) =
  if sum <= hd ns
  then 0
  else 1 + number_before_reaching_sum(sum - hd ns, tl ns)


fun what_month(day:int) =
  number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31])+1


fun month_range(day1:int, day2:int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)


fun oldest(dates:(int*int*int) list) =
  let fun oldest_helper(date:int*int*int, dates:(int*int*int) list) =
        if null dates
        then date 
        else if is_older(date, hd dates)
             then oldest_helper(date, tl dates)
             else oldest_helper(hd dates, tl dates) 
  in
    if null dates
    then NONE
    else SOME (oldest_helper(hd dates, tl dates))
  end


fun remove_duplicates(ns:int list) = 
  let 
    fun filter_dups(n, ns, seen) =
      if null ns
      then []
      else 
        if n = hd ns 
        then 
          if seen
          then filter_dups(n, tl ns, seen)
          else hd ns :: filter_dups(n, tl ns, true)
        else hd ns :: filter_dups(n, tl ns, seen)

    fun remove_helper(ns1, ns) = 
      if null ns1
      then ns
      else remove_helper(tl ns1, filter_dups(hd ns1, ns, false))
  in
    remove_helper(ns, ns)
  end


fun number_in_months_challenge(dates:(int*int*int) list, months:int list) =
  number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates:(int*int*int) list, months:int list) =
  dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date:int*int*int) = 
  let 
    fun leap_year(year) =
      if year mod 400 = 0 
      then true
      else if year mod 100 = 0 
           then false
           else year mod 4 = 0

    fun get_nth_days(n:int, month_days:int list) = 
      if n = 1
      then hd month_days
      else get_nth_days(n-1, tl month_days)

    fun valid_year(year:int) = year > 0

    fun valid_month(month:int) = month > 0 andalso month <= 12

    fun valid_day(year:int, month:int, day:int) = 
      day > 0 andalso if month = 2 andalso leap_year(year)
                      then day <= 29
                      else day <= get_nth_days(month, [31,28,31,30,31,30,31,31,30,31,30,31])
  in
    valid_year(#1 date) andalso valid_month(#2 date) andalso valid_day(#1 date, #2 date, #3 date)
  end
