open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

exception IndexError

(* Return the i'th element of a list *)
let rec nth (i : int)  (l: 'a list) : 'a =
  if i < 0 then raise IndexError else 
  match l with
    | [] -> raise IndexError
    | first::rest -> if i == 0 then first else nth (i-1) (rest) 

(* Append two lists *)
let rec append (l1 : 'a list) (l2: 'a list) : 'a list =
  match (l1, l2) with
    | ([], _) -> l2
    | (_, []) -> l1
    | (first1::rest1, first2::rest2) -> first1::append (rest1) (l2)
  

(* Reverse a list *)
let reverse (l : 'a list) : 'a list =
  let rec tail_reverse (l1 : 'a list) (l2: 'a list) : 'a list =
    match l1 with
      | [] -> l2
      | first::rest -> tail_reverse (rest) (first::l2)
  in tail_reverse (l) ([])


(* Length of a list *)
let rec length (l : 'a list) : int  =
  match l with
    | [] -> 0
    | first::rest -> 1 + length (rest)


(* Return the part of list l beginning at index 0 and ending at index
   iend *)
let rec list_prefix (iend : int) (l : 'a list) : 'a list =
  if iend < 0 then raise IndexError
  else if iend = 0 then []
  else
    match l with
      [] -> if iend = 0 then []
            else raise IndexError
    | a::d ->
       a :: list_prefix (iend-1) d

(* Return the part of list l beginning at istart and running through
   the end of the list *)
let rec list_suffix (istart : int) (l : 'a list) : 'a list =
  if istart < 0 then raise IndexError (*raise error if start is negative*)
  else if istart >= List.length l then raise IndexError (*raise error if istart is out of l index*)
  else if istart = 0 then l  (*Returns list at end of recursion*)
  else
    match l with
      [] -> 
        []  (*Returns empty list if list is empty*)
    | a::d -> 
        list_suffix (istart-1) d  (*Begins recursion until istart = 0*)


(* Merge sorted lists l1 and l2 based on cmp.  The result is a sorted
   list containing all elements from both l2 and l2. *)
let rec merge (cmp : 'a->'a->bool) (l1 : 'a list) (l2 : 'a list) : 'a list =
  (* TODO, replace [] *)
  []

(* Sort list l via mergesort

   cmp is a function that compares two elements of list l.  When cmp
   returns true, its first argument comes first in the sorted lest.
   When cmp returns false, its second argument comes first in the
   sorted list.  *)

let rec mergesort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  (* TODO, replace [] *)
  let len = length l in
  match len with
  | 0 -> []
  | 1 -> l
  | _ -> merge cmp (mergesort cmp (list_prefix (len/2) l)) (mergesort cmp (list_suffix (len/2) l)) 


(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)


let nth_tests =
  ("Nth", (fun (i,l)->nth i l), (=), (=),
   Some((fun x -> str_pair string_of_int str_int_list x),
        string_of_int),
   [
     (Some("simple list 1"), (0, [1;2;3;4;5]), Ok 1);
     (Some("negative index"), (-1, [1;2;3;4;5]), Error IndexError);
       (* TODO: Add more tests *)
     (Some("simple list 2"), (3, [1;2;3;4;5]), Ok 4);
     (Some("simple list 3"), (4, [1;2;3;4;5]), Ok 5);
     (Some("simple list 4"), (0, [1]), Ok 1);
     (Some("out of bounds"), (5, [1;2;3;4;5]), Error IndexError);
     (Some("empty list"), (0, []), Error IndexError);

  ])

let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list 1"), ([1;2],[3;4]), Ok [1;2;3;4]);
     (* TODO: Add more tests *)
     (Some("simple list 2"), ([1;2;3],[3;4]), Ok [1;2;3;3;4]);
     (Some("simple list 3"), ([1;2;3],[4]), Ok [1;2;3;4]);
     (Some("simple list 4"), ([1],[2]), Ok [1;2]);
     (Some("list 1 empty"), ([],[3;4]), Ok [3;4]);
     (Some("list 2 empty"), ([1;2;3],[]), Ok [1;2;3]);
     (Some("both lists empty"), ([],[]), Ok []);
  ])

let reverse_tests =
  ("reverse", reverse, (=), (=), Some(str_int_list,str_int_list),
   [
     (Some("simple list 1"), [1;2;3;4;5], Ok[5;4;3;2;1]);
       (* TODO: Add more tests *)
     (Some("simple list 2"), [1;2], Ok[2;1]);
     (Some("simple list 3"), [1;2;3], Ok[3;2;1]);
     (Some("longer list"), [1;2;3;4;5;6;7;8;9], Ok[9;8;7;6;5;4;3;2;1]);
     (Some("list with 1 element"), [1], Ok[1]);
     (Some("empty list"), [], Ok[]);
  ])

let length_tests =
  ("length", length, (=), (=), Some(str_int_list,string_of_int),
   [
     (Some("simple list 1"), [1;2;3;4;5], Ok 5);
       (* TODO: Add more tests *)
     (Some("simple list 2"), [1;2], Ok 2);
     (Some("simple list 3"), [1;2;3], Ok 3);
     (Some("longer list"), [1;2;3;4;5;6;7;8;9;10;11;12;13;14], Ok 14);
     (Some("small list"), [1], Ok 1);
     (Some("empty list"), [], Ok 0);
  ])

let list_prefix_tests =
  ("list_prefix", (fun (iend,l) -> list_prefix iend l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [1;2]);
     (None, (0,[1;2;3;4;5]), Ok []);
     (None, (4,[1;2;3;4;5]), Ok [1;2;3;4]);
     (None, (5,[1;2;3;4;5]), Ok [1;2;3;4;5]);
     (None, (-1,[1;2;3;4;5]), Error IndexError);
     (None, (6,[1;2;3;4;5]), Error IndexError);
     (None, (10,[1;2;3;4;5]), Error IndexError);
  ])

let list_suffix_tests =
  ("list_suffix", (fun (istart,l) -> list_suffix istart l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [3;4;5]);
       (* TODO: Add more tests *)
     (Some("negative istart"), (-3,[-12;-1;-4;-22]), Error IndexError);
     (Some("zero istart"), (0,[-12;-1;-4;-22]), Ok [-12;-1;-4;-22]);
     (Some("max istart"), (3,[-12;-1;-4;-22]), Ok [-22]);
     (Some("greater istart"), (4,[-12;-1;-4;-22]), Error IndexError);
     (Some("empty list"), (1,[]), Error IndexError);
  ])

let merge_tests =
  ("merge", (fun (cmp,l1,l2) -> merge cmp l1 l2), (=), (=),
   Some((fun (cmp,l1,l2) -> str_pair str_int_list str_int_list (l1, l2)),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3],[2;4;5]), Ok [1;2;3;4;5]);
       (* TODO: Add more tests *)
  ])


let mergesort_tests =
  ("mergesort", (fun (cmp,l) -> mergesort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3;4;2;5]), Ok [1;2;3;4;5]);
     (* DONE: Add more tests *)
     (Some("descending order"), ((>),[1;3;4;2;5]), Ok [5;4;3;2;1]);
     (Some("empty list"), ((<),[]), Ok[]);
     (Some("already descending order"), ((<),[5;4;3;2;1]), Ok [1;2;3;4;5]);
     (Some("duplicates"), ((<),[5;5;4;4;3;2;1]), Ok [1;2;3;4;4;5;5]);
     (Some("already sorted"), ((<),[1;2;3;4;5;6;7;8;9;10]), Ok [1;2;3;4;5;6;7;8;9;10]);
     (Some("negative elements"), ((<),[1;3;4;2;5;-23;-99;-3;-1]), Ok [-99;-23;-3;-1;1;2;3;4;5]);
   ])
