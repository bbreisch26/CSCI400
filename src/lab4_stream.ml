open Util (* see util.ml *)
open Lab4_lazy

(****************)
(* Starter Code *)
(****************)

module Stream = struct
  type 'a cell =
    | Cons of 'a * 'a t
    | Nil
  and 'a t = ('a cell) suspension

  exception IndexError

  (* Return the head element of the list *)
  let head(l : 'a t) : 'a  =
  (* DONE: replace `failwith "unimplemented"` *)
    match force l with
    | Cons (head, _) -> head
    | Nil -> failwith "empty"

  (* Return the tail of the list *)
  let tail(l : 'a t) : 'a t  =
  (* DONE: replace `failwith "unimplemented"` *)
    match force l with
    | Cons (_,tail) -> tail
    | Nil -> failwith "empty"

  let nil()  = delay(fun () -> Nil)

  (* Create stream from the elements of a list *)
  let rec from_list (l : 'a list) : 'a t =
  (* DONE: replace `failwith "unimplemented"` *)
    match l with
    | first::rest ->
       let susrest = delay (fun () -> from_list rest) in
       delay (fun () -> Cons (first, force susrest))
    | _ -> nil()

  (* Fold function f over the stream from the right (end) of the stream. *)
  (* fold_right is a monolithic function *)
  let rec fold_right (f : 'a->'acc->'acc) (s : 'a t) (acc : 'acc) : 'acc =
  (* TODO: replace `failwith "unimplemented"` *)
    match force s with
    | Cons (h,t) -> f h (fold_right f t acc)
    | Nil -> []

  let rec to_list s =
    fold_right (fun x l -> x :: l) s []

  (* Create a stream by applying function f to produce successive elements. *)
  (* applying f to element i will produce element i+1 *)
  let rec seed (f : 'a -> 'a) (x : 'a) =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"

  (* Create a stream by applying function f to produce successive elements. *)
  (* applying f to (x_i,y_i) will produce (x_{i+1}, y_{i+1}, where
     each x represents stream elements and each y some additional
     parameter *)
  let rec seed2 (f : ('a*'b) -> ('a*'b)) ((x,y) : ('a *'b)) =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"

  (* Append two streams *)
  let rec append (s1 : 'a t) (s2 : 'a t) : 'a t =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"

  (* Reverse a stream. *)
  (* reverse is a monolithic function *)
  let reverse (s : 'a t) : 'a t =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"

  let rec nth(l : 'a t) (n:int) : 'a =
    if 0 = n
    then head l
    else nth (tail l) (n-1)

  (* Return a stream containing the first n elements of the input stream *)
  let rec prefix (l:'a t) (n:int) : 'a t =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"

  let rec suffix (l:'a t) (n:int) : 'a t =
    if n <= 0 then l
    else suffix (tail l) (n-1)

  let rec sublist (l:'a t) (start:int) (stop:int) : 'a list =
    to_list (prefix (suffix l start) (stop - start))

  (* Map a function over a stream *)
  let rec map (f : 'x -> 'y) (l : 'x t): 'y t =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"


  (* Map a function over two streams *)
  let rec map2 (f : 'x -> 'y -> 'z) (l1 : 'x t) (l2 : 'y t)
          : 'z t =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"

  (* Filter a stream *)
  let rec filter  (f : 'x -> bool) (l : 'x t) : 'x t =
  (* TODO: replace `failwith "unimplemented"` *)
  failwith "unimplemented"


end

;;
(*********)
(* Tests *)
(*********)

(* to/from list *)
let stream_fromto_list_tester x = Stream.to_list (Stream.from_list x)
let stream_fromto_list_printer = Some(str_int_list,str_int_list)
let stream_fromto_list_tests =
  ("stream_fromto_list",
   stream_fromto_list_tester, (=), (=),
   stream_fromto_list_printer,
   [
     (None, [1;2;3], Ok [1;2;3]);
     (None, [1], Ok [1]);
     (None, [], Ok []);
  ])

(* seed tests *)
let stream_seed_tester (f,x,n) = Stream.to_list (Stream.prefix (Stream.seed f x) n)
let stream_seed_printer = Some((fun (f,x,n) -> string_of_int x), str_int_list)
let stream_seed_tests =
  ("seed",
   stream_seed_tester, (=), (=),
   stream_seed_printer,
   [
     (Some "nats", ((fun x -> 1+x), 0, 5), Ok [0;1;2;3;4]);
     (Some "xorshift32",
      ((fun x ->
        let bits32 = 0xffffffff in
        let x = ((x lsl 13) lxor x) land bits32 in
        let x = ((x lsr 17) lxor x) land bits32 in
        let x = ((x lsl  5) lxor x) land bits32 in
        x),
       42, 5),
      Ok [42; 11355432; 2836018348; 476557059; 3648046016]);
     (* TODO: fix the lambda function in the following test case *)
     (Some "zeno", ((fun x -> x), 256, 5), Ok [256;128;64;32;16]);
  ])

let stream_seed2_tester (f,xi,n) = Stream.to_list (Stream.prefix (Stream.seed2 f xi) n)
let stream_seed2_printer = Some((fun (f,xi,n) -> str_pair string_of_int string_of_int xi),
                                str_int_list)
let stream_seed2_tests =
  ("seed2",
   stream_seed2_tester, (=), (=),
   stream_seed2_printer,
   [
     (Some "fact", ((fun (x,i) -> (x*i,i+1)), (1,1), 5), Ok [1; 1; 2; 6; 24]);
     (* TODO: fix the lambda function in the following test case *)
     (Some "fib", ((fun (x,xm) -> (x,x)), (0,1), 8), Ok[0; 1; 1; 2; 3; 5; 8; 13] );
  ])

(* append *)

let stream_append_tester (a,b) =
  Stream.to_list(Stream.append
                    (Stream.from_list a)
                    (Stream.from_list b))
let stream_append_printer = Some(str_pair str_int_list str_int_list, str_int_list)
let stream_append_tests =
  ("stream_append",
   stream_append_tester, (=), (=),
   stream_append_printer,
   [
     (None, ([1;2;3],[4;5;6]), Ok [1;2;3;4;5;6]);
     (* TODO *)
  ])


(* reverse *)
let stream_reverse_tester x = Stream.to_list (Stream.reverse (Stream.from_list x))
                                let stream_reverse_printer = Some(str_int_list,str_int_list)
let stream_reverse_tests =
  ("stream_reverse_list",
   stream_reverse_tester, (=), (=),
   stream_reverse_printer,
   [
     (None, [1;2;3], Ok [3;2;1]);
     (* TODO *)
  ])


(* nth *)
let stream_nth_tester (x,n) = Stream.nth (Stream.from_list x) n
let stream_nth_printer = Some(str_pair str_int_list string_of_int,string_of_int)
let stream_nth_tests =
  ("stream_nth",
   stream_nth_tester, (=), (=),
   stream_nth_printer,
   [
     (None, ([0;1;2],1), Ok 1);
     (None, ([1;2;3],2), Ok 3);
     (None, ([1;2;3],3), Error Stream.IndexError);
     (None, ([],1), Error Stream.IndexError);
     (None, ([42],0), Ok 42);
  ])

(* prefix *)
let stream_prefix_tester (x,n) = Stream.to_list (Stream.prefix (Stream.from_list x) n)
let stream_prefix_printer = Some(str_pair str_int_list string_of_int,str_int_list)
let stream_prefix_tests =
  ("stream_prefix",
   stream_prefix_tester, (=), (=),
   stream_prefix_printer,
   [
     (None, ([0;1;2],2), Ok [0;1]);
     (* TODO *)
  ])

(* suffix *)
let stream_suffix_tester (x,n) = Stream.to_list (Stream.suffix (Stream.from_list x) n)
let stream_suffix_printer = Some(str_pair str_int_list string_of_int,str_int_list)
let stream_suffix_tests =
  ("stream_suffix",
   stream_suffix_tester, (=), (=),
   stream_suffix_printer,
   [
     (None, ([0;1;2],0), Ok [0;1;2]);
     (* TODO *)
  ])

(* map *)
let stream_map_tester (f,x) = Stream.to_list (Stream.map f (Stream.from_list x))
let stream_map_printer = Some((fun (f,x) -> str_int_list x), str_int_list)
let stream_map_tests =
  ("stream_map",
   stream_map_tester, (=), (=),
   stream_map_printer,
   [
     (None, ((fun x->1+x), [0;1;2]), Ok [1;2;3]);
     (* TODO *)
  ])

(* map2 *)
let stream_map2_tester (f,x,y) = Stream.to_list (Stream.map2 f (Stream.from_list x) (Stream.from_list y))
let stream_map2_printer = Some((fun (f,x,y) -> str_pair str_int_list str_int_list (x,y)), str_int_list)
let stream_map2_tests =
  ("stream_map2",
   stream_map2_tester, (=), (=),
   stream_map2_printer,
   [
     (None, ((+), [0;1;2], [4;5;6]), Ok [4;6;8]);
     (* TODO *)
  ])

(* filter *)
let stream_filter_tester (f,x) = Stream.to_list (Stream.filter f (Stream.from_list x))
let stream_filter_printer = Some((fun (f,x) -> str_int_list x), str_int_list)
let stream_filter_tests =
  ("stream_filter",
   stream_filter_tester, (=), (=),
   stream_filter_printer,
   [
     (None, ((fun x-> (x mod 2) = 0), [0;1;2;3;4;5;6]), Ok [0;2;4;6]);
     (* TODO *)
  ])
