(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (a, xs) =
    case xs of
	[] => NONE
      | x::xs' =>
	if same_string(a, x)
	then SOME xs'
	else case all_except_option(a, xs') of
		 NONE => NONE
	       | SOME z => SOME (x::z)
				
fun get_substitutions1 (xs, a) =
    case xs of
	[] => []
      | x::xs' => case all_except_option (a, x) of
		      NONE => get_substitutions1(xs', a)
		    | SOME z => z @ get_substitutions1(xs', a)

fun get_substitutions2 (xs, a) =
    let
	fun aux (xs, a, acc) =
	    case xs of
		[] => acc
	      | x::xs' => case all_except_option (a, x) of
			      NONE => aux (xs', a, acc)
			    | SOME z => aux (xs', a, z @ acc)
    in
	aux(xs, a, [])
    end

fun similar_names (xs, full_name) =
    let
	val {first=f,middle=m,last=l} = full_name
	fun fullname_generation xs =
	    case xs of
		[] => []
	      | x::xs' => {first=x,middle=m,last=l}::(fullname_generation xs')
				
    in
	full_name::fullname_generation(get_substitutions1(xs, f))			   
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
fun card_color (card) =
    case card of
	(Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

fun card_value (card) =
    case card of
	(_, Num n) =>  n
      | (_, Ace) => 11
      | (_, _) => 10 
			   
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | cs::cs' =>
	if cs = c
	then cs'
	else cs::remove_card(cs', c, e)

fun all_same_color (cs) =
    case cs of
	[] => true
      | [_] => true
      | c::cs::cs' => card_color c = card_color cs andalso all_same_color (cs::cs')

fun sum_cards cs =
    let
	fun aux (cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => aux(cs', card_value c + acc)
    in
	aux (cs, 0)
    end
	
fun score (cs, goal) =
    let
	val sum = sum_cards cs
	fun compute_score (sum, goal) =
	    if sum > goal
	    then (sum - goal) * 3
	    else goal - sum
			    
    in
	if all_same_color cs
	then compute_score (sum, goal) div 2
	else compute_score (sum, goal)		   
    end

fun officiate (card_list, move_list, goal) =
    let
	fun game (card_list, cards_held, moves) =
	    case moves of
		[] => cards_held
	      | head::tail =>
		case head of
		    Discard card => game(card_list, remove_card (cards_held, card, IllegalMove), tail)
		  | Draw  => case card_list of
				 [] => cards_held
			       | c::cs =>
				 if sum_cards (c::cards_held) > goal
				 then c::cards_held
				 else game(cs, c::cards_held, tail)
    in
	score (game (card_list, [], move_list), goal)
    end
	
					
									     
