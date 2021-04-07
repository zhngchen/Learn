fun number_before_reaching_sum(sum: int, int_lst: int list) = 
    if hd int_lst > sum
    then 1
    else 1 + number_before_reaching_sum(sum, (hd int_lst + hd (tl int_lst)) :: tl (tl int_lst))



fun month_range(day1: int, day2: int) =
    let val days_lst = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        if day1 > day2
        then []
        else number_before_reaching_sum(day1, days_lst) :: month_range(day1+1, day2)
    end 