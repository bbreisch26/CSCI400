open Util (* see util.ml *)

(*******************)
(* PART III: Ropes *)
(*******************)

module Rope = struct
  type rope =
    | Empty
    | Str of string (* leaf node: a string *)
    | Cat of int*int*rope*rope (* inner node: height, length, left, right *)

  let length (r : rope) : int =
    match r with
    | Empty -> 0
    | Str(s) -> String.length s
    | Cat(_,n,_,_)  -> n

  let rec height (r : rope) : int =
    match r with
    | Empty -> 0
    | Str(_) -> 1
    | Cat(h,_,_,_) -> h

  let to_string (r : rope) : string =
    let rec f buf i r =
      match r with
      | Empty -> i
      | Str(s) -> let n = String.length s
                  in (Bytes.blit_string s 0 buf i n; i+n)
      | Cat(_,n,l,r) -> f buf (f buf i l) r
    in
    match r with
    | Empty -> ""
    | Str(s) -> s
    | Cat(_,n,_,_) ->
       let buf = Bytes.create n in
       let _ = f buf 0 r in
       Bytes.to_string buf

  let from_string (s : string) : rope =
    if String.length s = 0
    then Empty
    else Str(s)

  (* Raise an exception for out of bounds access *)
  let out_of_bounds _ = invalid_arg "index out of bounds"

  (* Return the character at index i in r *)
  let rec get (r : rope) (i:int) : char =
    match r with
    | Empty -> out_of_bounds r
    | Str(s) -> String.get s i
    | Cat(_,_,l,r) ->
       (* DONE: replace 'x' *)
       match l with
       | Empty -> out_of_bounds l
       | Str(s) -> 
          if String.length s-1 < i then get (r) (i-String.length s)
          else String.get s i
       | Cat(_,s,_,_) ->
          if s-1 < i then get (r) (i-s)
          else get (l) (i)


  (* Test if the balance invariant does not hold because l is "too big" *)
  let toobig (hl : int) (hr : int) : bool = hl > 2*hr

  (* Test if x is balanced. Does not test if children are balanced. *)
  let is_balanced (x : rope) : bool =
    match x with
    | Empty | Str _ -> true
    | Cat(_,_,l,r) ->
       let hl,hr = height l, height r in
       not (toobig hl hr || toobig hr hl)

  (* Test if x is balanced and all decendants are balanced. *)
  let rec is_rec_balanced (x : rope) : bool =
    match x with
    | Empty | Str _ -> true
    | Cat(_,_,l,r) ->
       is_rec_balanced l && is_rec_balanced r && is_balanced x

  (* Create a new rope with left child l and right child r.  Both l
     and r must be balanced and the invariant must hold without any
     rebalancing of l and/or r.  That is:
     (height l <= 2 * height r) && (height r <= height l)
   *)
  let create (l : rope) (r : rope) : rope =
    match l,r with
    | Empty,_ -> r
    | _,Empty -> l
    | _ ->
       let hl,hr = height l,height r in
       if toobig hl hr then invalid_arg "Rope.create: left too big"
       else if toobig hr hl then invalid_arg "Rope.create: right too big"
       else let x = Cat(1+max hl hr, length l + length r, l, r) in
            if is_balanced x then x
            else invalid_arg "Rope.create: unbalanced result"

  (* Right rotation *)
  let rot_right(l : rope) (r : rope) : rope =
    (* DONE: replace Empty *)
    match l,r with
    | Cat(_,_,ll,lr),Str(_) -> create (ll) (create (lr) (r))
    | _ -> invalid_arg "Wrong rope types for right rotation"

  (* Right rotation, then left rotation *)
  let rot_right_left (l : rope) (r : rope) : rope =
    (* DONE: replace Empty *)
    match (l,r) with
    | (Cat(_,_,ll,lr),Str(_)) -> 
        (match (ll,lr) with
          | (Str(_),Cat(_,_,lrl,lrr)) -> create (create (ll) (lrl)) (create (lrr) (r))
          | (_,_) -> invalid_arg "Wrong rope types for right-left rotation")
    | (_,_) -> invalid_arg "Wrong rope types for right-left rotation"                

  (* Left rotation *)
  let rot_left (l : rope) (r : rope) : rope =
    (* DONE: replace Empty *)
    match l,r with
    | Str(_),Cat(_,_,rl,rr) -> create (create (l) (rl)) (rr)
    | _ -> invalid_arg "Wrong rope types for left rotation"

  (* Left rotation, then right rotation *)
  let rot_left_right (l : rope) (r : rope) : rope =
    (* DONE: replace Empty *)
    match l,r with
    | Str(_),Cat(_,_,rl,rr) ->
          (match rl,rr with
            | Cat(_,_,rll,rlr),Str(_) -> create (create (l) (rll)) (create (rlr) (rr))
            | _ -> invalid_arg "Wrong rope types for left-right rotation")
    | _ -> invalid_arg "Wrong rope types for left-right rotation"

  (* Same as create but performs one step of rebalancing if
     necessary. Assumes l and r are balanced and
     (height l - 1 <= 2 * height r) && (height r - 1 <= 2 * height l)
   *)
  let bal (l : rope) (r : rope) : rope =
    match l,r with
    | Empty,_  -> r
    | _,Empty  -> l
    | _ ->
       let hl,hr = height l,height r in
       if not (is_balanced l) then invalid_arg "Rope.bal: left unbalanced"
       else if not (is_balanced r) then invalid_arg "Rope.bal: right unbalanced"
       else if toobig (hl-1) hr then invalid_arg "Rope.bal: left too big"
       else if toobig (hr-1) hl then invalid_arg "Rope.bal: right too big"
       else
         (* TODO: Replace Empty *)
         if hl - hr > 1 then
          rot_right l r
         else if hr - hl > 1 then
          rot_left r l
         else
          create l r


  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)
  let rec cat (l : rope) (r : rope) : rope =
    (* TODO: Replace Empty *)
     match l with
     | Empty -> r
     | Str(s) -> bal (Str(s)) r
     | Cat(_,_,ll,lr) ->
      (match r with
       | Empty -> l
       | Str(s) -> bal l (Str(s))
       | Cat(_,_,rl,rr) -> bal (cat ll rl) (cat lr rr))


  (* Extract the subrope beginning at position pos and containing len
     number of characters *)
  let rec sub (r : rope) (pos:int) (len:int) : rope =
    match r with
    | Empty -> if pos = 0 && len = 0 then Empty
               else out_of_bounds r
    | Str(s) ->
       if pos + len > String.length s || pos < 0
       then out_of_bounds r
       else from_string(String.sub s pos len)
    | Cat(_,_,l,r) ->
       (* DONE *)
       if pos <= length l then
         sub l pos len
       else
         sub r (pos - length l) len

end

;;

(*********)
(* Tests *)
(*********)

let rope_repeat s i =
  let rec h i r =
    if i <= 0 then r
    else h (i-1) (Rope.cat r s)
  in h i Empty
;;


let rope_pair_printers = Some(str_pair Rope.to_string  Rope.to_string, Rope.to_string)

(* validate heights and lengths of test arguments *)
let rope_check_arg r =
  let rec f r =
    match r with
    | Rope.Empty | Rope.Str _ -> true
    | Rope.Cat(h,n,l,r) ->
       f l && f r
       && n = Rope.length l + Rope.length r
       && h = 1 + max (Rope.height l) (Rope.height r)
  in if f r then r
     else invalid_arg "test arg check"

let rope_pair_helper f : (Rope.rope*Rope.rope)->Rope.rope =
  (fun (l,r) -> f (rope_check_arg l) (rope_check_arg r))

(* Basic Tests (do not modify) *)

let to_string_tests =
  ("Rope.to_string",
   Rope.to_string, (=), (=),
   Some(Rope.to_string, str_str),
   [
     (None, Rope.Empty, Ok "");
     (None, Rope.Str "foo" , Ok "foo");
     (None, Rope.Cat(2,6,
                     Rope.Str "foo",
                     Rope.Str "bar"),
      Ok "foobar");
   ]
  )

let length_tests =
  ("Rope.length",
   Rope.length, (=), (=),
   Some(Rope.to_string, string_of_int),
   [
     (None, Rope.Empty, Ok 0);
     (None, Rope.Str "foo", Ok 3);
     (None, Rope.Cat(2,6,
                     Rope.Str "foo",
                     Rope.Str "bar"),
      Ok 6);
   ]
  )

let height_tests =
  ("Rope.height",
   Rope.height, (=), (=),
   Some(Rope.to_string, string_of_int),
   [
     (None, Rope.Empty, Ok 0);
     (None, Rope.Str "foo", Ok 1);
     (None, Rope.Cat(2,6,
                     Rope.Str "foo",
                     Rope.Str "bar"),
      Ok 2);
   ]
  )

(* Rotations *)
let rot_right_helper = rope_pair_helper Rope.rot_right
let rot_right_tests =
  ("Rope.rot_right",
   rot_right_helper,
   (=), (=),
   rope_pair_printers,
   [
     (None,
      (Rope.Cat(2,4, Rope.Str "ll", Rope.Str "lr"),
       Rope.Str "r"),
      Ok (Rope.Cat(3,5,
                   Rope.Str "ll",
                   Rope.Cat(2,3,Rope.Str "lr", Rope.Str "r")))
     );
     (* DONE *)
     (Some("Two strings error"), 
       (Rope.Str "l",
       Rope.Str "r"),
       Error (Invalid_argument "Wrong rope types for right rotation")
     );
     (Some("Two Cats error"),
       (Rope.Cat(2,4, Rope.Str "ll", Rope.Str "lr"),
       Rope.Cat(2,4, Rope.Str "rl", Rope.Str "rr")),
       Error (Invalid_argument "Wrong rope types for right rotation")
     );
   ]
  )


let rot_right_left_helper = rope_pair_helper Rope.rot_right_left
let rot_right_left_tests =
  ("Rope.rot_right_left",
   rot_right_left_helper,
   (=), (=),
   rope_pair_printers,
   [
     (None,
      (Rope.Cat(3,8,
                Rope.Str "ll",
                Rope.Cat(2,6,
                          Rope.Str"lrl",
                          Rope.Str"lrr")),
       Rope.Str "r"),
      Ok (Rope.Cat(3,9,
                   Rope.Cat(2,5, Rope.Str "ll", Rope.Str "lrl"),
                   Rope.Cat(2,4,Rope.Str "lrr", Rope.Str "r")))
     );
     (* TODO *)
   ]
  )

let rot_left_helper = rope_pair_helper Rope.rot_left
let rot_left_tests =
  ("Rope.rot_left",
   rot_left_helper,
   (=), (=),
   rope_pair_printers,
   [
     (None,
      (Rope.Str "l",
       Rope.Cat(2,4, Rope.Str "rl", Rope.Str "rr")),
      Ok (Rope.Cat(3,5,
                   Rope.Cat(2,3,Rope.Str "l", Rope.Str "rl"),
                   Rope.Str "rr"))
     );
     (* TODO *)
   ]
  )

let rot_left_right_helper = rope_pair_helper Rope.rot_left_right
let rot_left_right_tests =
  ("Rope.right_rot_left",
   rot_left_right_helper,
   (=), (=),
   rope_pair_printers,
   [
     (None,
      (Rope.Str "l",
       Rope.Cat(3,8,
                Rope.Cat(2,6,
                         Rope.Str "rll",
                         Rope.Str "rlr"),
                Rope.Str "rr")),
      Ok (Rope.Cat(3,9,
                   Rope.Cat(2,4, Rope.Str "l", Rope.Str "rll"),
                   Rope.Cat(2,5,Rope.Str "rlr", Rope.Str "rr")))
     );
     (* TODO *)
   ]
  )

(* create *)
let create_helper = rope_pair_helper Rope.create
let create_ok_list = [
    (None,
     (Rope.Empty, Rope.Empty),
     Ok (Rope.Empty));
    (None,
     (Rope.Empty, Rope.Str "x"),
     Ok (Rope.Str "x"));
    (None,
     (Rope.Str "x", Rope.Empty),
     Ok (Rope.Str "x"));
    (None,
     (Rope.Str "x", Rope.Str "y"),
     Ok (Rope.Cat(2, 2, Rope.Str "x", Rope.Str "y")));
    (* TODO: tests also used for balance and cat *)
  ]
let create_all_list =
  create_ok_list
  @ [
      (None,
       (Rope.Cat(3,3,
                 Rope.Cat(2,2,Rope.Str "x", Rope.Str "y"),
                 Rope.Str "z"),
        Rope.Str "w"),
       Error (Invalid_argument "Rope.create: left too big"));
      (None,
       (Rope.Str "w",
        Rope.Cat(3,3,
                 Rope.Cat(2,2,Rope.Str "x", Rope.Str "y"),
                 Rope.Str "z")),
       Error (Invalid_argument "Rope.create: right too big"));
    ]
let create_tests =
  ("Rope.create",
   create_helper,
   (=), (=),
   rope_pair_printers,
   create_all_list
  )

(* balance *)
let bal_helper = rope_pair_helper Rope.bal
let bal_ok_list =
  create_ok_list (* handle everything Rope.create does *)
  @ [ (* and more *)
    (None,
     (Rope.Empty, Rope.Empty),
     Ok (Rope.Empty));
    (None,
     (Rope.Empty, Rope.Str "x"),
     Ok (Rope.Str "x"));
    (None,
     (Rope.Str "x", Rope.Empty),
     Ok (Rope.Str "x"));
    (None,
     (Rope.Str "x", Rope.Str "y"),
     Ok (Rope.Cat(2, 2, Rope.Str "x", Rope.Str "y")));
    ]
let bal_all_list =
  bal_ok_list
  @ [
      (* TODO *)
      
    ]
let bal_tests =
  ("Rope.bal",
   bal_helper,
   (=), (=),
   rope_pair_printers,
   bal_all_list
  )

(* cat *)
let cat_helper = rope_pair_helper Rope.cat
let cat_list =
  bal_ok_list (* handle everything Rope.bal does *)
  @ [ (* and more *)
      (* TODO *)
    ]

let cat_tests =
  ("Rope.cat",
   cat_helper,
   (=), (=),
   rope_pair_printers,
   cat_list
  )

(* get *)
let get_helper (r,i) = Rope.get r i
let get_printer =
  Some(str_pair Rope.to_string  string_of_int, (fun c -> Printf.sprintf "%c" c))
let get_tests =
  ("Rope.get",
   get_helper, (=), (=),
   get_printer,
   [
     (None, (Rope.Str "foo", 0), Ok 'f');
     (* DONE *)
     (Some("Left Side"), (Rope.Cat(2, 6, Rope.Str "abc", Rope.Str "def"), 1), Ok 'b');
     (Some("Right Side"), (Rope.Cat(2, 6, Rope.Str "abc", Rope.Str "def"), 4), Ok 'e');
     (Some("Left then Left"), (Rope.Cat(3, 9, 
                                        Rope.Cat(2, 6, Rope.Str "abc", Rope.Str "def"),
                                        Rope.Str "ghi"),
                               0), Ok 'a');
     (Some("Left then Right"), (Rope.Cat(3, 9,
                                         Rope.Cat(2, 6, Rope.Str "abc", Rope.Str "def"),
                                         Rope.Str "ghi"),
                                5), Ok 'f');
     (Some("Right then Left"), (Rope.Cat(3, 9, Rope.Str "abc",
                                         Rope.Cat(2, 6, Rope.Str "def", Rope.Str "ghi")),
                                3), Ok 'd'); 
     (Some("Right then Right"), (Rope.Cat(3, 9, Rope.Str "abc",
                                          Rope.Cat(2, 6, Rope.Str "def", Rope.Str "ghi")),
                                 6), Ok 'g');
   ]
  )

(* sub *)
let sub_helper (r,pos,len) = Rope.sub r pos len
let sub_printer =
  Some((fun(r,pos,len) ->
      "(" ^ (Rope.to_string r) ^  ","
      ^ (string_of_int pos) ^ ","
      ^ (string_of_int len) ^ ")"),
       Rope.to_string)
let sub_tests =
  ("Rope.sub",
   sub_helper, (=), (=),
   sub_printer,
   [
     (None, (Rope.Str "foo", 1, 2),
      Ok (Rope.Str "oo"));
     (* DONE *)
     (None, (Rope.Str "foo", 0, 3), 
      Ok (Rope.Str "foo"));
     (None, (Rope.Str "foo", 1, 2), 
      Ok (Rope.Str "oo"));
     (None, (Rope.Str "foo", 2, 1), 
      Ok (Rope.Str "o"));
     (None, (Rope.Str "foo", 3, 0), 
      Ok (Rope.Empty));
     (None, (Rope.Str "stringoutofbounds", 20, 1),
      Error (Invalid_argument "index out of bounds"));
     (None, (Rope.Str "stringoutofbounds", -10, 20),
      Error (Invalid_argument "index out of bounds"));
   ]
  )
