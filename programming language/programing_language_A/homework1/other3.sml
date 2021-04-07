fun is_older(date1 : int * int * int, date2 : int * int * int) = 
    if (#1 date1 <> #1 date2)then
        (#1 date1 < #1 date2)
    else if (#2 date1 <> #2 date2)then
        (#2 date1 < #2 date2)
    else if (#3 date1 <> #3 date2)then
        (#3 date1 < #3 date2)
    else false


fun number_in_month(days : (int*int*int) list, month : int) = 
    let
        fun evalMonth(days:(int*int*int) list, month:int, count:int) = 
            if null days then 
                count
            else if #2 (hd days) = month then
                evalMonth(tl days, month, count+1)
            else
                evalMonth(tl days, month, count)
    in
      evalMonth(days, month, 0)
    end


fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates then 
        [] 
    else 
        if #2 (hd dates) = month then 
            (hd dates) :: dates_in_month(tl dates, month)
        else
            dates_in_month(tl dates, month)



fun dates_in_months (dates : (int*int*int) list, months: int list) = 
    if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)


fun date_to_string(date : int*int*int) =
    get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)


fun number_before_reaching_sum(sum : int, numList : int list) = 
    if hd numList >= sum 
    then 0
    else 1 + number_before_reaching_sum(sum - hd numList, tl numList)


fun what_month (date : int) = 
    1 + number_before_reaching_sum(date, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])


fun month_range (day1 : int, day2 : int) = 
    if day1 <= day2 
    then what_month(day1) :: month_range(day1+1, day2)
    else []


fun oldest(dateList : (int*int*int) list) = 
    if null dateList 
    then NONE
    else 
        let val tl_ans = oldest(tl dateList)
        in  if isSome tl_ans andalso is_older(valOf tl_ans, hd dateList)
            then tl_ans
            else SOME(hd dateList)
        end



