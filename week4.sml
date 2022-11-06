(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs =
	List.filter (fn x => Char.isUpper(String.sub (x, 0))) xs

fun longest_string1 xs =
    List.foldl (fn (first, second) => if String.size first > String.size second
					then first
					else second)
	       "" xs

fun longest_string2 xs =
    List.foldl (fn (first, second) => if String.size first >= String.size second
					then first
					else second)
	       "" xs

fun longest_string_helper f xs =
    List.foldl (fn (first, second) => if f(String.size first, String.size second)
				      then first
				      else second)
	       ""xs
	       

val longest_string3 = longest_string_helper (fn (first, second) => first > second)		  
val longest_string4 = longest_string_helper (fn (first, second) => first >= second)

val longest_capitalized  = longest_string3 o only_capitals
val rev_string = implode o rev o explode

fun first_answer f l =
    case l of
	[] => raise NoAnswer
      | x::xs => case f x of
		     NONE => first_answer f xs
		   | SOME ans => ans

fun all_answers f l =
	let
	    fun helper acc l =
		case l of
		    [] => SOME acc
		  | x::xs => case f x of
				 NONE => NONE
			       | SOME ans => helper (acc@ans) xs
	in
	    helper [] l
	end
	    

val count_wildcards = g (fn () => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun count_some_var (str, pattern) =
    g (fn _ => 0) (fn x => if x = str then 1 else 0) pattern

fun check_pat p =
    let
	fun all_strings p = 
			case p of
			    Variable x 		=> [x]
			  | TupleP ps           => List.foldl (fn (p, i) => all_strings p @ i) [] ps 
			  | ConstructorP (_, p) => all_strings p 
			  | _  					=> []

	fun no_repeats strings =
			case strings of
			    []  => true
			  | x::xs =>  not (List.exists (fn s => s = x) xs)  andalso no_repeats xs
    in
	no_repeats (all_strings p)
    end

fun match (v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
	  | (_, Variable s)   => SOME [(s, v)]
	  | (Unit, UnitP)     => SOME []
	  | (Const c1, ConstP c) => if c = c1 then SOME [] else NONE
	  | (Tuple vs, TupleP ps) => if length vs = length ps 
				     then all_answers match (ListPair.zip (vs, ps))
				     else NONE
					      
	  | (Constructor (s2, v), ConstructorP (s1, p1)) => if s1 = s2 then match (v, p1) else NONE
	  | _  => NONE

fun first_match v ps = 
	SOME (first_answer (fn p => match (v, p)) ps)
	handle NoAnswer => NONE
			       
