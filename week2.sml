fun is_older (d1 : int * int * int, d2 : int * int * int) =
    if (#1 d1) < (#1 d2)
    then true
    else if (#1 d1) > (#1 d2)
    then false
    else
	if (#2 d1) < (#2 d2)
	then true
	else if (#2 d1) > (#2 d2)
	then false
	else
	    if (#3 d1) < (#3 d2)
	    then true
	    else false
 

fun number_in_month(d : (int * int * int) list, m : int) =
    if null d
    then 0
    else
	if #2 (hd d) = m
	then 1 + number_in_month (tl d, m) 
	else 0 + number_in_month (tl d, m) 
	    
fun number_in_months(d : (int * int * int) list, m : int list) =
    if null m
    then 0
    else number_in_month(d, hd m) + number_in_months(d, tl m)

fun dates_in_month(d : (int * int * int) list, m : int) =
    if null d
    then []
    else
	if #2 (hd d) = m
	then (hd d)::dates_in_month (tl d, m)
	else dates_in_month (tl d, m)

fun dates_in_months(d : (int * int * int) list, m : int list) =
    if null m
    then []
    else dates_in_month(d, hd m) @ dates_in_months(d, tl m)

fun get_nth (str : string list, n : int) =
    if n = 1
    then hd str
    else get_nth(tl str, n - 1)
		
fun date_to_string (d : int * int * int) =
    let
	val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val n = #2 d
    in
	get_nth(month_names, n) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end

fun number_before_reaching_sum (sum : int, lst : int list) =
    if null lst
    then 0
    else
	if hd lst < sum
	then 1 + number_before_reaching_sum(sum - hd lst, tl lst)
	else 0
		 
fun what_month(day : int) =
    let
	val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (day, months) + 1
    end

fun month_range (d1 : int, d2 : int) =
    if d1 > d2
    then []
    else
	if d1 <= d2
	then (what_month d1)::(month_range(d1 + 1,d2))
	else []

fun oldest(d : (int * int * int) list) =
    if null d
    then NONE
    else
	let
	    val tl_d = oldest(tl d)
	in
	    if isSome tl_d andalso is_older(valOf tl_d, hd d)
	    then tl_d
	    else SOME (hd d)
	end

fun not_unique(m : int, lst : int list) =
    if null lst
    then false
    else
	m = hd lst orelse not_unique(m, tl lst)
				 

fun remove_duplicate(m : int list) =
    if null m
    then []
    else
	 let
	     val tl_m = remove_duplicate(tl m)
	 in
	     if not_unique(hd m, tl_m)
	     then tl_m
	     else (hd m)::tl_m
	 end
	
	    
fun number_in_months_challenge(d : (int * int * int) list, m : int list) =
    if null m
    then 0
    else
	let
	    val new_list = remove_duplicate(m)
	in  
	    number_in_months(d, new_list)
	end

fun dates_in_months_challenge(d : (int * int * int) list, m : int list) =
    if null m
    then []
    else
	let
	    val new_list = remove_duplicate(m)
	in  
	    dates_in_months(d, new_list)
	end

fun get_nth_int (str : int list, n : int) =
    if n = 1
    then hd str
    else get_nth_int(tl str, n - 1)
		
fun year_check(y : int) =
    y > 0

fun month_check (m : int) =
    if m >= 1 andalso m <= 12
    then true
    else false


fun leap_year (y : int) =
    if (y mod 400 = 0 orelse y mod 4 = 0) andalso y mod 100 <> 0
    then true
    else false
	     
fun date_check (d : int, m : int, y : int) =
    if d < 1
    then false
    else
	let
	    val month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    val month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
	    if leap_year(y)
	    then
		if d <= get_nth_int(month_leap, m)
		then true
		else false
	    else
		if d <= get_nth_int(month, m)
		then true
		else false
	end
	    
	
fun reasonable_date(d : int * int * int) =
    let
	val year = #1 d
	val month = #2 d
	val date = #3 d
    in
	if not (year_check(year))
	then false
	else
	    if not (month_check (month))
	    then false
	    else
		if date_check (date, month, year)
		then true
		else false
    end
