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
    | Nil -> raise IndexError

  (* Return the tail of the list *)
  let tail(l : 'a t) : 'a t  =
  (* DONE: replace `failwith "unimplemented"` *)
    match force l with
    | Cons (_,tail) -> tail
    | Nil -> raise IndexError

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
  (* DONE: replace `failwith "unimplemented"` *)
    match force s with
    | Cons (h,t) -> f h (fold_right f t acc)
    | Nil -> []

  let rec to_list s =
    fold_right (fun x l -> x :: l) s []

  (* Create a stream by applying function f to produce successive elements. *)
  (* applying f to element i will produce element i+1 *)
  let rec seed (f : 'a -> 'a) (x : 'a) =
    (* DONE: replace `failwith "unimplemented"` *)
   delay (fun () -> Cons(x, seed f (f x)))
  

  (* Create a stream by applying function f to produce successive elements. *)
  (* applying f to (x_i,y_i) will produce (x_{i+1}, y_{i+1}, where
     each x represents stream elements and each y some additional
     parameter *)
  let rec seed2 (f : ('a*'b) -> ('a*'b)) ((x,y) : ('a *'b)) =
    (* TODO: replace `failwith "unimplemented"` *)
    delay(fun() -> Cons(x, seed2 f (f (x, y))))
    

  (* Append two streams *)
  let rec append (s1 : 'a t) (s2 : 'a t) : 'a t =
  (* DONE: replace `failwith "unimplemented"` *)
    match force s1, force s2 with
      | Nil, Nil -> nil()
      | Cons(_,_), Nil -> s1
      | Nil, Cons(_,_) -> s2
      | Cons(h,r), Cons(_,_) -> delay(fun () -> Cons(h, append (r) (s2)))

  (* Reverse a stream. *)
  (* reverse is a monolithic function *)
  let reverse (s : 'a t) : 'a t =
  (* DONE: replace `failwith "unimplemented"` *)
    let rec reverse_helper acc l =
      match force l with
        | Nil -> acc
        | Cons(h,r) -> reverse_helper (h::acc) (r)
    in 
    let revList = reverse_helper ([]) (s) in 
    from_list revList
         

  let rec nth(l : 'a t) (n:int) : 'a =
    if 0 = n
    then head l
    else nth (tail l) (n-1)

  (* Return a stream containing the first n elements of the input stream *)
  let rec prefix (l:'a t) (n:int) : 'a t =
  (* DONE: replace `failwith "unimplemented"` *)
    if n <= 0 then (if force l == Nil then failwith "Stream.prefix: Stream too short" else nil())
    else
      match force l with
        | Nil -> failwith "Stream.prefix: Stream too short"
        | Cons(h,r) -> delay(fun () -> Cons(h,prefix (r) (n-1)))

  let rec suffix (l:'a t) (n:int) : 'a t =
    if n <= 0 then l
    else suffix (tail l) (n-1)

  let rec sublist (l:'a t) (start:int) (stop:int) : 'a list =
    to_list (prefix (suffix l start) (stop - start))

  (* Map a function over a stream *)
  let rec map (f : 'x -> 'y) (l : 'x t): 'y t =
  (* DONE: replace `failwith "unimplemented"` *)
    match force l with
      | Nil -> nil()
      | Cons(h,r) -> delay(fun () -> Cons(f h, map f r))

  (* Map a function over two streams *)
  let rec map2 (f : 'x -> 'y -> 'z) (l1 : 'x t) (l2 : 'y t)
          : 'z t =
  (* DONE: replace `failwith "unimplemented"` *)
    match force l1,force l2 with
      | Nil, Nil -> nil()
      | Nil, Cons(_,_) -> failwith "Stream.map2: Different Sized Streams"
      | Cons(_,_), Nil -> failwith "Stream.map2: Different Sized Streams"
      | Cons(h1,r1), Cons(h2,r2) -> delay(fun () -> Cons(f h1 h2, map2 f r1 r2)) 



  (* Filter a stream *)
  let rec filter  (f : 'x -> bool) (l : 'x t) : 'x t =
  (* DONE: replace `failwith "unimplemented"` *)
    match force l with
      | Nil -> nil()
      | Cons(h,r) -> 
         if f h then 
           delay(fun () -> Cons(h, filter f r))
         else
           filter f r


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
     (* DONE: fix the lambda function in the following test case *)
     (Some "zeno", ((fun x -> x/2), 256, 5), Ok [256;128;64;32;16]);
     (Some "zero", ((fun x -> x), 0, 5), Ok [0;0;0;0;0]);
     (Some "square", ((fun x -> x*x), 2, 4), Ok [2;4;16;256]);
     (Some "fixed", ((fun x-> 123), 0, 4), Ok [0;123;123;123]);
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
     (* DONE: fix the lambda function in the following test case *)
     (Some "fib", ((fun (x,xm) -> (xm,x+xm)), (0,1), 8), Ok[0; 1; 1; 2; 3; 5; 8; 13] );
     (Some "powers2", ((fun (x,y) -> (x*y,y)), (1,2), 4), Ok[1;2;4;8]);
     (Some "fixed", ((fun (x,y) -> (x,y)), (0,1), 4), Ok[0;0;0;0]);
     (Some "swap", ((fun (x,y) -> (y,x)), (0,1), 4), Ok[0;1;0;1]);
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
     (* DONE *)
     (Some "Two Empty Streams", ([],[]), Ok []);
     (Some "First List Empty", ([],[1;2;3]), Ok [1;2;3]);
     (Some "Second List Empty", ([1;2;3],[]), Ok [1;2;3]);
     (Some "Long Lists", ([1;2;3;4;5;6;7;8;9;10],[11;12;13;14;15;16;17;18;19;20]), Ok [1;2;3;4;5;6;7;8;9;10;11;12;13;14;        15;16;17;18;19;20]);
     (Some "Long and Short Lists", ([1;2;3;4;5;6;7;8;9;10],[11;12]), Ok [1;2;3;4;5;6;7;8;9;10;11;12]);
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
     (* DONE *)
     (Some "Empty Stream", [], Ok []);
     (Some "Short Stream", [1;2], Ok [2;1]);
     (Some "Long Stream", [1;2;3;4;5;6;7;8;9;10], Ok [10;9;8;7;6;5;4;3;2;1]);
     (Some "Duplicate Values", [1;2;2;3;3;4;5;6;6;7], Ok [7;6;6;5;4;3;3;2;2;1]);
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
     (* DONE *)
     (Some "Empty Stream", ([],2), Error (Failure "Stream.prefix: Stream too short"));
     (Some "n Value of 0", ([1;2;3;4;5],0), Ok []);
     (Some "Empty Stream and n Value of 0", ([],0), Error (Failure "Stream.prefix: Stream too short"));
     (Some "Long Stream", ([1;2;3;4;5;6;7;8;9;10],9), Ok [1;2;3;4;5;6;7;8;9]);
     (Some "Stream Too Short", ([1;2;3;4;5],6), Error (Failure "Stream.prefix: Stream too short"));
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
     (* DONE *)
     (Some "Empty Stream", ([],1), Error (Failure "empty"));
     (Some "Long Stream Short Suffix", ([1;2;3;4;5;6;7;8;9;10],8), Ok [9;10]);
     (Some "Long Stream Long Suffix", ([1;2;3;4;5;6;7;8;9;10],2), Ok [3;4;5;6;7;8;9;10]);
     (Some "Stream Too Short", ([1;2;3;4;5],6), Error (Failure "empty"));
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
     (* DONE *)
     (Some "Map with Subtraction", ((fun x -> x - 2), [1;2;3;4;5]), Ok [-1;0;1;2;3]);
     (Some "Map with Multiplication", ((fun x -> x * 5), [1;2;3;4;5]), Ok [5;10;15;20;25]);
     (Some "Map with Division", ((fun x -> x / 5), [5;10;15;20;25]), Ok [1;2;3;4;5]); 
     (Some "Map with Addition", ((fun x -> x + 2), [1;2;3;4;5]), Ok [3;4;5;6;7]);

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
     (* DONE *)
     (Some "Empty Streams", ((+), [], []), Ok []);
     (Some "Only First Stream Empty", ((+), [], [1;2;3]), Error (Failure "Stream.map2: Different Sized Streams"));
     (Some "Only Second Stream Empty", ((+), [1;2;3], []), Error (Failure "Stream.map2: Different Sized Streams"));
     (Some "Map2 with Addition", ((+), [1;2;3;4;5], [2;3;4;5;6]), Ok [3;5;7;9;11]);
     (Some "Map2 with Subtraction", ((-), [1;2;3;4;5], [2;3;4;5;6]), Ok [-1;-1;-1;-1;-1]);
     (Some "Map2 with Mutiplication", ((fun x y -> x * y), [1;2;3;4;5], [1;2;3;4;5]), Ok [1;4;9;16;25]);
     (Some "Map2 with Division", ((/), [1;2;3;4;5], [1;2;3;4;5]), Ok [1;1;1;1;1]);
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
     (* DONE *)
     (Some "Empty Stream", ((fun x -> x == 0), []), Ok []);
     (Some "Duplicate Values", ((fun x -> (x mod 2) == 0), [0;0;1;1;2;2;3;3;4;4;5;5]), Ok [0;0;2;2;4;4]);
     (Some "Filters No Elements", ((fun x -> x > 0), [1;2;3;4;5;6;7;8;9;10]), Ok [1;2;3;4;5;6;7;8;9;10]);
     (Some "Filters Every Element", ((fun x -> x < 0), [1;2;3;4;5;6;7;8;9;10]), Ok []);
  ])
