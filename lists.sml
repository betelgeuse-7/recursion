(* Tuples: arbitrarily long and deep, can hold different type of values
   Lists:  arbitrarily long and deep, canNOT hold different type of values *)


val empty_list = []
val list1 = [1, 2, 4, 6]

(* null function: 
        list -> boolean 
        return true if the list is empty; otherwise false.*)

(* hd function:
        list -> first element of the list *)

(* tl function:
        list -> list
        return a list consisting of every element in the list except for ' hd list ' *)

(* cons :: *)

val list2 = 99::list1; (* [99,1,2,4,6] : int list *)

(* int list -> int list *)
(* return a list with elements 1 bigger than before. *)


fun make_bigger_by_one(list: int list) = 
    if null list 
    then []
    else 
        hd (list) + 1 :: make_bigger_by_one (tl(list))


fun sum_list (xs: int list) = 
    if null xs
    then 0
    else 
        hd(xs) + sum_list (tl(xs))

    (*  Logic
        [1, 2, 3, 4, 5]
        1 + [2, 3, 4, 5]
        3 + [3, 4, 5]
        6 + [4, 5]
        10 + [5]
        15 + 0 (null) = 15
    *)

fun countdown_list (x: int) = 
    if x = 0
    then []
    else 
        x :: countdown_list (x - 1)

(* int list * int list -> int list *)
fun append (xs: int list, ys: int list) = 
    if null xs 
    then ys
    else (hd xs) :: append ((tl xs), ys)

    (*
    Logic
    [1, 2, 3] [4, 5, 6]
    1 :: [2, 3] [4, 5, 6]
    1 :: 2 :: [3] [4, 5, 6]
    1 :: 2 :: 3 :: [] [4, 5, 6]
    if xs null -> ys
    1 :: 2 :: 3 :: [4, 5, 6]
    1 :: 2 :: [3, 4, 5, 6]
    1 :: [2, 3, 4, 5, 6]
    [1, 2, 3, 4, 5, 6]
    *)

fun sum_pair_list (xs: (int * int) list) = 
    if null xs
    then 0
    else 
       #1 (hd (xs)) + #2 (hd (xs)) + sum_pair_list (tl (xs))
    
    (*
        Logic
        [(1, 2), (3, 4), (5, 6)]
        1 + 2 + [(3, 4), (5, 6)]
        1 + 2 + 3 + 4 + [(5, 6)]
        1 + 2 + 3 + 4 + 5 + 6 + []
        1 + 2 + 3 + 4 + 5 + 6 + 0
        21
    *)

fun firsts (xs: (int * int) list) = 
    (* base case *)
    if null xs 
    then []
    else 
        #1 (hd (xs)) :: firsts (tl (xs))

fun seconds (xs: (int * int) list) = 
    (* base case *)
    if null xs 
    then []
    else 
        #2 (hd (xs)) :: seconds (tl (xs))

(* sum_pair_list using helper functions *)
fun sum_pair_list2 (xs: (int * int) list) = 
    sum_list (firsts(xs)) + sum_list (seconds (xs))

fun list_product (xs: int list) = 
    if null xs 
    then 1
    else 
        hd (xs) * list_product (tl (xs))

fun factorial (n : int) =
    if n = 0
    then 1
    else 
        n * factorial (n - 1)


(* factorial using helper functions *)
fun factorial2 (n : int) = 
    list_product (countdown_list (n))
