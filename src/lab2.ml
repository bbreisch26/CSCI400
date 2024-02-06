open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

(*** Implementing higher-order functions ***)

let rec map (f : 'a->'b) (l : 'a list) : 'b list =
  (* DONE, replace [] *)
  match l with
  | [] -> []
  | first::rest -> f (first)::map (f) (rest)

let rec filter (f : 'a->bool) (l : 'a list) : 'a list =
  (* TODO, replace l *)
  l

let rec fold_left (f: 'y ->'x->'y) (y:'y) (l:'x list) : 'y =
  match l with
  | [] -> y
  | first::rest -> (fold_left f (f y first) rest) 

let rec fold_right (f : 'x->'y->'y) (y:'y) (l:'x list) : 'y =
  match l with
  | [] -> y
  | first::rest -> f (first) (fold_right (f) (y) (rest))


(*** Using higher-order functions ***)


(* Concatenate two lists. *)
let append (l1 : 'a list) (l2 : 'a list) : 'a list =
  (*fold_left (let cons y x = x) l2 l1*)
  []
  

(* rev_append l1 l2 reverses l1 and concatenates it with l2 *)
let rev_append (l1 : 'a list) (l2 : 'a list) : 'a list =
  (* TODO, replace [] *)
  []

(* Concatenate a list of lists. *)
let flatten (l : 'a list list) : 'a list =
  (* TODO, replace [] *)
  []


(* Insertion Sort *)

(* Insert elt into sorted list l in sorted order *)
let rec insert (cmp : 'a->'a->bool) (elt :'a) (l:'a list) : 'a list =
  (* TODO, replace [] *)
  []

let insertionsort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  (* TODO, replace l *)
  l


(* Selection Sort *)

(* Select the initial element from l based on cmp.  Return a tuple of
   the initial element and the list with the initial element
   removed. *)
let select (cmp : 'a->'a->bool) (l:'a list) : 'a * 'a list =
  match l with
  | [] -> invalid_arg "select"
  | a::d ->
     (* TODO, replce (a,d) *)
     (a,d)

let rec selectionsort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  (* TODO, replace l *)
  l


(* Quicksort *)

(* Partion list l around elt.  Return a tuple consisting of all
   elements before elt and all elements after elt. *)
let pivot (cmp : 'a->'a->bool) (elt :'a) (l:'a list) : 'a list * 'a list =
  (* TODO, replace ([],[]) *)
  ([], [])

(* The simple implementation of quicksort recurses on the two sublists
   and appends the sorted results. *)
let rec quicksort_simple (cmp : 'a->'a->bool) (l : 'a list) : 'a list =
  (* TODO, replace l *)
  l

(* The better implementation of quicksort elides the append by passing
   a "tail" list to recursive calls.  Sorted results are directly
   cons'ed onto the tail, avoiding the need for an extra append. *)
let quicksort_better (cmp : 'a->'a->bool) (l : 'a list) : 'a list =
  let rec f (cmp : 'a->'a->bool) (l : 'a list) (r : 'a list) : 'a list =
    (* r is the tail: everything that must come after l in the sorted
       list. Passing r to f saves us from having to append sorted
       lists. *)
    (* TODO, replace l @ r *)
    l @ r
  in f cmp l []

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let list_cmp cmp l1 l2 =
  (List.sort cmp l1) = (List.sort cmp l2)

let int_list_cmp l1 l2 =
  list_cmp (-) l1 l2


let map_tests =
  ("map", (fun (f,l)->map f l), (=), (=),
   Some((fun (f,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((fun x -> 1+x), [1;2;3;4;5]), Ok [2;3;4;5;6]);
       (* TODO: Add more tests *)
     (Some("simple list multiplication"), ((fun x -> x*2), [1;2;3;4;5]), Ok [2;4;6;8;10]);
     (Some("empty list"), ((fun x -> x+1), []), Ok []);
     (Some("long list addition of 5"), ((fun x -> x+5), [1;2;3;4;5;6;7;8;9;10]), Ok [6;7;8;9;10;11;12;13;14;15]);
     (Some("simple list division"), ((fun x -> x/2), [2;4;6;8;10]), Ok [1;2;3;4;5]);
     (Some("simple list subtraction"), ((fun x -> x-4), [10;15;20;25;30]), Ok [6;11;16;21;26]);
  ])

let filter_tests =
  ("filter", (fun (f,l)->filter f l), (=), (=),
   Some((fun (f,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((fun x -> (x mod 2)=0), [1;2;3;4;5]), Ok [2;4]);
       (* TODO: Add more tests *)
  ])

let fold_left_tests =
  ("fold_left", (fun (f,y,l)->fold_left f y l), (=), (=),
   Some((fun (f,y,l) -> str_pair string_of_int str_int_list (y,l)),
        string_of_int),
   [
     (Some("+"), ((+), 0, [1;2;3]), Ok 6);
     (Some("-"), ((-), 0, [1;2;3]), Ok (-6));
     (Some("+.init"), ((+), 2, [1;2;3;]), Ok 8);
     (*(Some("append"), (((fun cons x y -> x::y)), [1;2;3;], [4;5;6;]), Ok [1;2;3;4;5;6;]);*)
       (* TODO: Add more tests *)

  ])

let fold_right_tests =
  ("fold_right", (fun (f,y,l)->fold_right f y l), (=), (=),
   Some((fun (f,y,l) -> str_pair string_of_int str_int_list (y,l)),
        string_of_int),
   [
     (Some("+"), ((+), 0, [1;2;3]), Ok 6);
     (Some("-"), ((-), 0, [1;2;3]), Ok 2);
     (* TODO: Add more tests *)
     (Some("+ with initial val"), ((+), 6, [1;2;3]), Ok 12);
     (Some("- with initial val"), ((-), 6, [1;2;3]), Ok (-4));
     (Some("Quiz6Question5"), ((fun a b -> a), 0, [1;2;3]), Ok (1));
     (Some("Quiz6Question7"), ((fun a b -> b), 0, [1;2;3]), Ok (0));
     (Some("+ empty list"), ((+), 6, []), Ok 6);
     (Some("- empty list"), ((-), 6, []), Ok 6);
     (Some("*"), (( * ), 6, [1;2;3]), Ok 36);
     (Some("/"), (( / ), 5, [10;20;50]), Ok 5);
  ])


let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [1;2;3;4]);
       (* TODO: Add more tests *)
  ])

let rev_append_tests =
  ("rev_append", (fun (l1,l2)->rev_append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [2;1;3;4]);
       (* TODO: Add more tests *)
  ])

let flatten_tests =
  ("flatten", (fun l -> flatten l), (=), (=),
   Some((fun l -> "[" ^ str_x_list (str_int_list) l ";" ^ "]" ),
        str_int_list),
   [
     (Some("simple list"), [[1;2];[3;4]], Ok [1;2;3;4]);
     (Some("simple list 2"), [[3;4]; [1;2]], Ok [3;4;1;2]);
     (* TODO: Add more tests *)
   ]
  )


let sort_test_cases = [
    (Some("simple list"), ((<),[1;3;4;2;5]), Ok [1;2;3;4;5]);
    (* TODO: Add more tests *)
  ]

let insert_tests =
  ("insert", (fun (cmp,elt,l)->insert cmp elt l), (=), (=),
   Some(((fun (cmp,elt,l) -> str_pair string_of_int str_int_list (elt,l)),
         (fun y -> str_int_list y)
     )),
   [
     (Some("simple <"), ((<), 0, [-1;1;2]), Ok ([-1; 0; 1; 2]));
     (Some("simple >"), ((>), 0, [2;1;-1]), Ok ([2; 1; 0; -1]));
     (* TODO: Add more tests *)
   ])

let insertionsort_tests =
  ("insertionsort", (fun (cmp,l) -> insertionsort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)


let select_test_eq (s1,l1) (s2,l2) =
  (s1 = s2) && (int_list_cmp l1 l2)

let select_tests =
  ("select", (fun (cmp,l)->select cmp l), select_test_eq, (=),
   Some(((fun (cmp,l) -> str_int_list l),
         (fun (s,l) -> str_pair string_of_int str_int_list (s,l))
     )),
   [
     (Some("simple <"), ((<), [1;-1;2]), Ok (-1,[2;1]));
     (Some("simple >"), ((>), [1;-1;2]), Ok (2,[1;-1]));
     (* TODO: Add more tests *)
   ])


let selectionsort_tests =
  ("selectionsort", (fun (cmp,l) -> selectionsort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)


let pivot_test_eq (a1,b1) (a2,b2) =
  (int_list_cmp a1 a2) && (int_list_cmp b1 b2)

let pivot_tests =
  ("pivot", (fun (cmp,elt,l)->pivot cmp elt l), pivot_test_eq, (=),
   Some(((fun (cmp,elt,l) -> str_pair string_of_int str_int_list (elt,l)),
         (fun y -> str_pair str_int_list  str_int_list y)
     )),
   [
     (Some("simple <"), ((<), 0, [-1;1;0;-2; 2]), Ok ([-2; -1],[2; 0; 1]));
     (Some("simple >"), ((>), 0, [-1;1;0;-2; 2]), Ok ([2; 1], [-2; 0; -1]));
     (* TODO: Add more tests *)
  ])

let quicksort_simple_tests =
  ("quicksort_simple", (fun (cmp,l) -> quicksort_simple cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)

let quicksort_better_tests =
  ("quicksort_better", (fun (cmp,l) -> quicksort_better cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   sort_test_cases)
