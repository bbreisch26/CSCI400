open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

exception IndexError

(* Return the i'th element of a list *)
let rec nth (i : int)  (l: 'a list) : 'a =
  match l with
    [] -> raise IndexError
  | a::d -> a (* TODO, replace the a *)

(* Append two lists *)
let rec append (l1 : 'a list) (l2: 'a list) : 'a list =
  (* TODO, replace [] *)
  []

(* Reverse a list *)
let reverse (l : 'a list) : 'a list =
  (* TODO, replace [] *)
  []

(* Length of a list *)
let length (l : 'a list) : int  =
  (* TODO, replace 0 *)
  0


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
  (* TODO, replace [] *)
  []


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
     (Some("simple list"), (0, [1;2;3;4;5]), Ok 1);
     (Some("error"), (-1, [1;2;3;4;5]), Error IndexError);
       (* TODO: Add more tests *)
  ])

let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [1;2;3;4]);
       (* TODO: Add more tests *)
  ])

let reverse_tests =
  ("reverse", reverse, (=), (=), Some(str_int_list,str_int_list),
   [
     (Some("simple list"), [1;2;3;4;5], Ok[5;4;3;2;1]);
       (* TODO: Add more tests *)
  ])

let length_tests =
  ("length", length, (=), (=), Some(str_int_list,string_of_int),
   [
     (Some("simple list"), [1;2;3;4;5], Ok 5);
       (* TODO: Add more tests *)
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
     (* TODO: Add more tests *)
   ])
