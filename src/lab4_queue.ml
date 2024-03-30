open Util (* see util.ml *)
open Lab4_lazy
open Lab4_stream

(****************)
(* Starter Code *)
(****************)

module Queue = struct

  (* These queues are based on Okasaki's BankersQueue, with the
     optimization noted on p.64 of using an ordinary list for the rear
     elements. *)

  (* front-length, front-elements, rear-length, rear-elements *)
  type 'a t =  int * 'a Stream.t * int * 'a list

  let empty() : 'a t  = (0, Stream.nil (), 0, [])

  exception Empty


  let rotate (q: 'a t) : 'a t  =
    let rec append_rev (f: 'a Stream.t) (r : 'a list) : 'a Stream.t  =
      (* return a stream of f followed by the reverse of r.  Only
         realize the reversal of r when we reach the end of f *)
        match force f with 
        | Stream.Nil ->  Stream.from_list (List.rev r)
        | Stream.Cons (first, rest) -> delay (fun() -> Stream.Cons (first, (append_rev rest r)))
    in
    match q with
    | (lenf, f, lenr, r) -> (lenf+lenr, append_rev f r, 0, [])

  (* Invariant: rear length <= front length
     Implied Invariant: a non-empty queue always has a non-empty front stream *)
  let check (q:'a t) : 'a t =
    match q with
    | (lenf, f, lenr, r) -> if lenr <= lenf then q else rotate q

  let is_empty (q : 'a t) =
    let (lenf, _, _, _) = q in (lenf=0)

  let snoc (q:'a t) (x:'a) : 'a t =
    match q with
    | (lenf, f, lenr, r) -> check (lenf, f, lenr+1, x::r)


  let head (q:'a t) : 'a =
  (* DONE: replace `failwith "unimplemented"` *)
    match q with
    | (_, a, _, _) ->
      match force a with
      | Cons(x, r) -> x
      | Nil -> raise Empty 
  

  let tail (q:'a t) : 'a t =
  (* DONE: replace `failwith "unimplemented"` *)
    match q with
    | (a, b, c, d) ->
      match force b with
      | Cons(h, r) -> check (a-1, r, c, d)
      | Nil -> raise Empty

  let rec fold_right (f : 'a -> 'acc -> 'acc ) (q : 'a t) (acc : 'acc) : 'acc =
  (* DONE: replace `failwith "unimplemented"` *)
    match q with
    | (a, b, c, d) -> Stream.fold_right f b acc

  let from_lists (f : 'a list) (r : 'a list) : 'a t =
    (List.length f, Stream.from_list f, List.length r, r)

  let to_lists (q:'a t) : (int*'a list*int*'a list)  =
    let (lf, f, lr, r) = q in
    (lf, Stream.to_list f, lr, r)

  let from_list (l : 'a list) : 'a t =
    List.fold_left snoc (empty()) l

  let rec to_list (q : 'a t) : 'a list =
    fold_right (fun x l -> x :: l) q []

end

;;

(*********)
(* Tests *)
(*********)

(* to/from list *)
let queue_fromto_list_tester x = Queue.to_list (Queue.from_list x)
let queue_fromto_list_printer = Some(str_int_list,str_int_list)
let queue_fromto_list_tests =
  ("queue_fromto_list",
   queue_fromto_list_tester, (=), (=),
   queue_fromto_list_printer,
   [
     (None, [1;2;3], Ok [1;2;3]);
     (None, [1], Ok [1]);
     (None, [], Ok []);
  ])

(* to/from lists *)
let queue_fromto_lists_tester (f,r) =
  Queue.to_lists (Queue.from_lists f r)

let queue_lists_printer =
  str_tuple4 string_of_int str_int_list string_of_int str_int_list

let queue_fromto_lists_printer =
  Some(str_pair str_int_list str_int_list,
       queue_lists_printer)

let queue_fromto_lists_tests =
  ("queue_fromto_lists",
   queue_fromto_lists_tester, (=), (=),
   queue_fromto_lists_printer,
   [
     (None, ([1;2;3],[4;5]), Ok (3,[1;2;3],2,[4;5]));
     (None, ([1;2;3],[4;5;6]), Ok (3,[1;2;3],3,[4;5;6]));
     (None, ([1;2;3],[]), Ok (3,[1;2;3],0,[]));
     (None, ([],[]), Ok (0,[],0,[]));
  ])



(* head *)
let queue_head_tester (f,r) =
  Queue.head (Queue.from_lists f r)
let queue_head_printer =
  Some(str_pair str_int_list str_int_list,
       string_of_int)
let queue_head_tests =
  ("queue_head",
   queue_head_tester, (=), (=),
   queue_head_printer,
   [
     (None, ([3;4],[4;5]), Ok 3);
     (* DONE *)
     (None, ([3],[4;5]), Ok 3);
     (None, ([3;4],[]), Ok 3);
     (None, ([],[4;5]), Error Queue.Empty);
     (None, ([3;4;5;6;7],[8]), Ok 3);
  ])

(* check *)
let queue_check_tester (f,r) =
  Queue.to_lists (Queue.check (Queue.from_lists f r))
let queue_check_printer =
  Some(str_pair str_int_list str_int_list, queue_lists_printer)
let queue_check_tests =
  ("queue_check",
   queue_check_tester, (=), (=),
   queue_check_printer,
   [
     (None, ([2;3],[5;4]), Ok(2,[2;3],2,[5;4]));
     (* DONE *)
     (None, ([1],[3;2]), Ok(3,[1;2;3],0,[]));
     (None, ([],[3;2]), Ok(2,[2;3],0,[]));
     (None, ([1;2;3],[5;4]), Ok(3,[1;2;3],2,[5;4]));
     (None, ([1;2;3],[]), Ok(3,[1;2;3],0,[]));
  ])

(* snoc *)
let queue_snoc_tester (f,r,x) =
  Queue.to_lists (Queue.snoc (Queue.from_lists f r) x)
let queue_snoc_printer =
  Some(str_tuple3 str_int_list str_int_list string_of_int,
       queue_lists_printer)
let queue_snoc_tests =
  ("queue_snoc",
   queue_snoc_tester, (=), (=),
   queue_snoc_printer,
   [
     (None, ([1;2],[3],4), Ok (2,[1;2],2,[4;3]));
     (* DONE *)
     (None, ([1;2;3],[4],5), Ok (3,[1;2;3],2,[5;4]));
     (None, ([],[],1), Ok (1,[1],0,[]));
     (None, ([1],[],2), Ok (1,[1],1,[2]));
     (None, ([1;2],[4;3],5), Ok (5,[1;2;3;4;5],0,[]));
  ])

(* tail *)
let queue_tail_tester (f,r) =
  Queue.to_lists (Queue.tail (Queue.from_lists f r))
let queue_tail_printer =
  Some(str_pair str_int_list str_int_list, queue_lists_printer)
let queue_tail_tests =
  ("queue_tail",
   queue_tail_tester, (=), (=),
   queue_tail_printer,
   [
     (None, ([1;2],[4;3]), Ok(3,[2;3;4],0,[]));
     (* DONE *)
     (None, ([],[4;3]), Error Queue.Empty);
     (None, ([1;2],[]), Ok(1,[2],0,[]));
     (None, ([1],[4;3;2]), Ok(3,[2;3;4],0,[]));
     (None, ([1;2;3],[4]), Ok(2,[2;3],1,[4]));
  ])
