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
    match (l,r) with
    | (Cat(_,_,ll,lr),_) ->
       let rotated_r = Cat(1+max (height r) (height lr), length lr + length r, lr, r) in
       Cat(1+max (height rotated_r) (height ll),length l + length r,ll,rotated_r)
    | _ -> invalid_arg "Wrong rope types for right rotation"

  (* Right rotation, then left rotation *)
  let rot_right_left (l : rope) (r : rope) : rope =
    (* DONE: replace Empty *)
    match (l,r) with
    | (Cat(_,_,ll,lr),_) -> 
       (match (ll,lr) with
        | (_,Cat(_,_,lrl,lrr)) ->
           let rotated_ll = Cat(1+max (height ll) (height lrl), (length ll) + (length lrl), ll, lrl) in
           let rotated_l = Cat(1+max (height rotated_ll) (height lrr), (length rotated_ll) + (length lrr), rotated_ll, lrr) in
           (rot_right rotated_l r) 
          | (_,_) -> invalid_arg "Wrong rope types for right-left rotation")
    | (_,_) -> invalid_arg "Wrong rope types for right-left rotation"                

  (* Left rotation *)
  let rot_left (l : rope) (r : rope) : rope =
    (* DONE: replace Empty *)
    match l,r with
    | _,Cat(_,_,rl,rr) ->
       let rotated_l = Cat(1+max (height l) (height rl), (length l) + (length rl), l, rl) in
       Cat(1+max (height rotated_l) (height rr), (length l) + (length r), rotated_l, rr)
    | _ -> invalid_arg "Wrong rope types for left rotation"

  (* Left rotation, then right rotation *)
  let rot_left_right (l : rope) (r : rope) : rope =
    (* DONE: replace Empty *)
    match l,r with
    | _,Cat(_,_,rl,rr) ->
       (match rl,rr with
        | Cat(_,_,rll,rlr),_ ->
           let rotated_rr = Cat(1+max (height rr) (height rlr), (length rr) + length(rlr), rlr, rr) in
           let rotated_r = Cat(1+max (height rotated_rr) (height rll), (length rotated_rr) + (length rll), rll, rotated_rr) in
           (rot_left l rotated_r)
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
    | Str(s), Str(sr) -> (*Went back and added this as early check for if both ropes are just str*)
      create l r
    | _ -> 
       let hl,hr = height l,height r in
       if not (is_balanced l) then invalid_arg "Rope.bal: left unbalanced"
       else if not (is_balanced r) then invalid_arg "Rope.bal: right unbalanced"
       else if toobig (hl-1) hr then invalid_arg "Rope.bal: left too big"
       else if toobig (hr-1) hl then invalid_arg "Rope.bal: right too big"
       else
        (*Continue match here, it's not pretty but TA advised me to do it like this*)
         match l,r with
         | Cat(hl, llen, ll, lr), Cat(hr, rlen, rl, rr) ->
          if toobig hl hr then
            rot_right l r
          else if toobig hr hl then
            rot_left l r
          else
            create l r
         | Cat(hl, llen, ll, lr), Str(s) ->
          if toobig hl hr then
            rot_right l r
          else
            create l r
         | Str(s), Cat(hr, rlen, rl, rr) ->
          if toobig hr hl then
            rot_left l r
          else
            create l r
          


  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)
  let rec cat (l : rope) (r : rope) : rope =
     match l, r with
     | Empty,Empty -> Empty
     | Empty,_ -> r
     | _, Empty -> l
     | Str(s),Str(x) -> create l r 
     | Str(s),Cat(h,len,rl,rr) -> Cat (h + 1, len + String.length s, (cat l rl), rr)
     | Cat(h,len,ll,lr),Str(s) -> Cat (h + 1, len + String.length s, (cat r lr), ll)
     | Cat(_,_,ll,lr),Cat(rl,rr,_,_) ->
      let hl,hr = height l,height r in
        if toobig hl hr then
          rot_right l r
        else if toobig hr hl then
          rot_left l r
        else create l r


  (* Extract the subrope beginning at position pos and containing len
     number of characters *)
  let rec sub (r : rope) (pos:int) (len:int) : rope =
    if pos < 0 then out_of_bounds r
    else
    match r with
    | Empty -> if pos = 0 && len = 0 then Empty
               else out_of_bounds r
    | Str(s) ->
       if pos + len > String.length s
       then out_of_bounds r
       else from_string(String.sub s pos len)
    | Cat(_,_,l,r) ->
       (* DONE *)
       if pos + len <= length l then
         sub l pos len
       else if pos >= length l then
         sub r (pos - length l) len
       else
         (*Recurse across tree, pos/len spans subropes*)
         let left_sub_rope_len = length l - pos in
         let left_sub_rope = sub l pos left_sub_rope_len in
         let right_sub_rope = sub r 0 (len - left_sub_rope_len) in
         let new_height = max (height left_sub_rope) (height right_sub_rope) in
         match (left_sub_rope, right_sub_rope) with
         | (Cat(_,_,_,_), _)
         | (_, Cat(_,_,_,_)) -> Cat (new_height + 1, len, left_sub_rope, right_sub_rope)
         | _ -> Cat(new_height, len, left_sub_rope, right_sub_rope)
  
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
     (Some("Two Rope.Cats"),
       (Rope.Cat(2,4, Rope.Str "he", Rope.Str "ll"),
       Rope.Cat(2,6, Rope.Str "ow", Rope.Str "orld")),
       Ok (Rope.Cat(4,10, Rope.Str("he"),
                    Rope.Cat(3, 8, Rope.Str("ll"),
                             Rope.Cat(2,6, Rope.Str("ow"), Rope.Str("orld")))))
     );
     (Some("non-str lr child"),
      (Rope.Cat(3,6, Rope.Str("He"), Rope.Cat(2,4,Rope.Str("ll"),Rope.Str("ow"))),
       Rope.Cat(2,4, Rope.Str("or"), Rope.Str("ld"))),
      Ok (Rope.Cat(4,10, Rope.Str("He"), Rope.Cat(3, 8, Rope.Cat(2,4,Rope.Str("ll"),Rope.Str("ow")), Rope.Cat(2,4,Rope.Str("or"),Rope.Str("ld"))))));
     (Some("Two Emptys error"),
       (Rope.Empty, Rope.Empty),
       Error (Invalid_argument "Wrong rope types for right rotation")
     );
     (Some("Test with longer strings"),
       (Rope.Cat(2, 5, Rope.Str "Hel", Rope.Str "lo"),
       Rope.Str "World"),
       Ok (Rope.Cat(3, 10,
                   Rope.Str "Hel",
                   Rope.Cat(2, 7, Rope.Str "lo", Rope.Str "World")))
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
     (* Done *)
     (Some("two empty ropes - error"),
      (Rope.Empty, Rope.Empty),
      Error(Invalid_argument "Wrong rope types for right-left rotation"));
     (Some("One string rope - error"),
      (Rope.Cat(2,4,Rope.Str("ab"),Rope.Str("cd")), Rope.Str("ef")),
      Error(Invalid_argument "Wrong rope types for right-left rotation"));
     (Some("Longer left child"),
      (Rope.Cat(3,6,Rope.Str("he"),Rope.Cat(2,4,Rope.Str("ll"),Rope.Str("ow"))),
       Rope.Cat(2,4,Rope.Str("or"),Rope.Str("ld"))),
      Ok(Rope.Cat(4,10,
                  Rope.Cat(2,4,Rope.Str("he"),Rope.Str("ll")),
                  Rope.Cat(3,6,Rope.Str("ow"),
                           Rope.Cat(2,4,Rope.Str("or"),Rope.Str("ld"))))));
     (Some("Stupid tall tree"),
      (Rope.Cat(3,12,Rope.Cat(2,6,Rope.Str("lll"),Rope.Str("llr")),
                Rope.Cat(2,6,Rope.Str("lrl"),Rope.Str("lrr"))),
       Rope.Cat(3,12,Rope.Cat(2,6,Rope.Str("rll"),Rope.Str("rlr")),
                Rope.Cat(2,6,Rope.Str("rrl"),Rope.Str("rrr")))),
      Ok(Rope.Cat(5,24,Rope.Cat(3,9,Rope.Cat(2,6,Rope.Str("lll"),Rope.Str("llr")),
                                Rope.Str("lrl")),
                  Rope.Cat(4,15,Rope.Str("lrr"),
                           Rope.Cat(3,12,Rope.Cat(2,6,Rope.Str("rll"),Rope.Str("rlr")),
                                    Rope.Cat(2,6,Rope.Str("rrl"),Rope.Str("rrr")))))));
     (Some("Not deep enough for rotation"),
      (Rope.Cat(2,2,Rope.Str("a"),Rope.Str("b")),
       Rope.Cat(2,2,Rope.Str("c"),Rope.Str("d"))),
      Error(Invalid_argument "Wrong rope types for right-left rotation"));
     
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
     (* DONE *)
     (Some("Two strings error"),
       (Rope.Str "l", Rope.Str "r"),
       Error (Invalid_argument "Wrong rope types for left rotation")
     );
     (Some("Two Rope.Cats"),
       (Rope.Cat(2,4,Rope.Str "he",Rope.Str "ll"),
        Rope.Cat(2,6,Rope.Str "ow",Rope.Str "orld")),
       Ok (Rope.Cat(4,10, Rope.Cat(3,6,Rope.Cat(2,4,Rope.Str("he"),Rope.Str("ll")),Rope.Str("ow")),Rope.Str("orld")))
     );
     (Some("Rope.Cat rl child"),
      (Rope.Cat(2,4,Rope.Str("he"),Rope.Str("ll")),
       Rope.Cat(3,6,Rope.Cat(2,4,Rope.Str("ow"),Rope.Str("or")),Rope.Str("ld"))),
      Ok(Rope.Cat(4,10, Rope.Cat(3,8,
                                 Rope.Cat(2,4,Rope.Str("he"),Rope.Str("ll")),
                                 Rope.Cat(2,4,Rope.Str("ow"),Rope.Str("or"))),
                        Rope.Str("ld"))));
     (Some("Two Emptys error"),
       (Rope.Empty, Rope.Empty),
       Error (Invalid_argument "Wrong rope types for left rotation")
     );
     (Some("Test with longer strings"),
       (Rope.Str "Hel",
        Rope.Cat(2,7,Rope.Str "lo", Rope.Str "World")),
       Ok (Rope.Cat(3,10,
                    Rope.Cat(2,5,Rope.Str "Hel", Rope.Str "lo"),
                    Rope.Str "World"))
     );
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
     (Some("two empty ropes - error"),
      (Rope.Empty, Rope.Empty),
      Error(Invalid_argument "Wrong rope types for left-right rotation"));
     (Some("One string rope - error"),
      (Rope.Cat(2,4,Rope.Str("ab"),Rope.Str("cd")), Rope.Str("ef")),
      Error(Invalid_argument "Wrong rope types for left-right rotation"));
     (Some("longer right child"),
      (Rope.Cat(2,4,Rope.Str("he"),Rope.Str("ll")),
       Rope.Cat(3,6,Rope.Cat(2,4,Rope.Str("ow"),Rope.Str("or")),Rope.Str("ld"))),
      Ok(Rope.Cat(4,10,Rope.Cat(3,6,Rope.Cat(2,4,Rope.Str("he"),Rope.Str("ll")),Rope.Str("ow")),Rope.Cat(2,4,Rope.Str("or"),Rope.Str("ld")))));
     (Some("Stupid tall tree"),
      (Rope.Cat(3,12,Rope.Cat(2,6,Rope.Str("lll"),Rope.Str("llr")),
                Rope.Cat(2,6,Rope.Str("lrl"),Rope.Str("lrr"))),
       Rope.Cat(3,12,Rope.Cat(2,6,Rope.Str("rll"),Rope.Str("rlr")),
                Rope.Cat(2,6,Rope.Str("rrl"),Rope.Str("rrr")))),
      Ok(Rope.Cat(5,24,Rope.Cat(4,15,Rope.Cat(3,12,Rope.Cat(2,6,Rope.Str("lll"),Rope.Str("llr")),
                                              Rope.Cat(2,6,Rope.Str("lrl"),Rope.Str("lrr"))),
                                Rope.Str("rll")),
                  Rope.Cat(3,9,Rope.Str("rlr"),Rope.Cat(2,6,Rope.Str("rrl"),Rope.Str("rrr"))))));
     (Some("Not deep enough"),
      (Rope.Cat(2,2,Rope.Str("a"),Rope.Str("b")),
       Rope.Cat(2,2,Rope.Str("c"),Rope.Str("d"))),
      Error(Invalid_argument "Wrong rope types for left-right rotation"));
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
    (* DONE: tests also used for balance and cat *)
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
      (None,
       (Rope.Str "ax", Rope.Str "by"),
        Ok (Rope.Cat(2, 4, Rope.Str "ax", Rope.Str "by")));
      (None,
       (Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b"), Rope.Str "c"),
        Ok (Rope.Cat(3, 3, Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b"), Rope.Str "c")));
      (None,
       (Rope.Str "a", Rope.Cat(2, 2, Rope.Str "b", Rope.Str "c")),
        Ok (Rope.Cat(3, 3, Rope.Str "a", Rope.Cat(2, 2, Rope.Str "b", Rope.Str "c"))));
      (None,
       (Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b"), Rope.Cat(2, 2, Rope.Str "c", Rope.Str "d")),
        Ok (Rope.Cat(3, 4, Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b"), 
                           Rope.Cat(2, 2, Rope.Str "c", Rope.Str "d"))));
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
      (* TODO*)
      (None,
       (Rope.Str "xa", Rope.Str "yb"),
       Ok (Rope.Cat(2, 4, Rope.Str "xa", Rope.Str "yb")));
      (None,
       (Rope.Str "x", Rope.Cat(4, 4, 
                        Rope.Cat(3, 3, Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b")
                          , Rope.Str "c")
                            , Rope.Str "d")),
       Error (Invalid_argument "Rope.bal: right unbalanced"));
      (None,
       (Rope.Cat(4, 4, 
                        Rope.Cat(3, 3, Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b")
                          , Rope.Str "c")
                            , Rope.Str "d"), Rope.Str "x"),
       Error (Invalid_argument "Rope.bal: left unbalanced"));
      (None,
       (Rope.Cat(4, 8, 
                        Rope.Cat(3, 4, Rope.Cat(2, 2,Rope.Str "a", Rope.Str "b"), Rope.Cat(2, 2,Rope.Str "c", Rope.Str "d")),
                        Rope.Cat(3, 4, Rope.Cat(2, 2,Rope.Str "e", Rope.Str "f"), Rope.Cat(2, 2,Rope.Str "g", Rope.Str "h")))
                        , Rope.Str "x"),
       Error (Invalid_argument "Rope.bal: left too big"));
      (None,
       (Rope.Str "x", Rope.Cat(4, 8, 
                               Rope.Cat(3, 4, Rope.Cat(2, 2,Rope.Str "a", Rope.Str "b"), Rope.Cat(2, 2,Rope.Str "c", Rope.Str "d")),
                        Rope.Cat(3, 4, Rope.Cat(2, 2,Rope.Str "e", Rope.Str "f"), Rope.Cat(2, 2,Rope.Str "g", Rope.Str "h")))
                        ),
       Error (Invalid_argument "Rope.bal: right too big"));
      (None,
       (Rope.Str "x", Rope.Cat(3, 4, 
                        Rope.Cat(2, 2,Rope.Str "a", Rope.Str "b"), Rope.Cat(2, 2,Rope.Str "c", Rope.Str "d"))
                        ),
       Ok (Rope.Cat(4,5, Rope.Cat(3,3,Rope.Str("x"),Rope.Cat(2,2,Rope.Str("a"),Rope.Str("b"))), Rope.Cat(2,2,Rope.Str("c"),Rope.Str("d")))));

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
      (None,
       (Rope.Str "xa", Rope.Str "yb"),
       Ok (Rope.Cat(2, 4, Rope.Str "xa", Rope.Str "yb")));

      (None,
       (Rope.Cat(4, 4, 
                        Rope.Cat(3, 3, Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b")
                          , Rope.Str "c")
                            , Rope.Str "d"), Rope.Str "x"),
       Ok (Rope.Cat(5, 5,
                    Rope.Cat(2,2,Rope.Str "x",Rope.Str "d")
                    , Rope.Cat(3,3,
                               Rope.Cat(2,2,Rope.Str "a",Rope.Str "b"),
                               Rope.Str "c")))
       );

      (None,
       (Rope.Str "x", Rope.Cat(4, 4, 
                        Rope.Cat(3, 3, Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b")
                          , Rope.Str "c")
                            , Rope.Str "d")),
       Ok (Rope.Cat(5, 5,
                    Rope.Cat(4,4,
                             Rope.Cat(3,3, 
                                      Rope.Cat(2,2, Rope.Str "x", Rope.Str "a"),
                                      Rope.Str "b"),
                             Rope.Str "c"),
                    Rope.Str "d")));
                    
      (None,
       (Rope.Str "x", Rope.Cat(4, 5, 
                        Rope.Cat(3, 3, Rope.Cat(2, 2, Rope.Str "a", Rope.Str "b")
                          , Rope.Str "c")
                            , Rope.Cat(2, 2, Rope.Str "d", Rope.Str "e"))),
       Ok (Rope.Cat(5, 6,
                    Rope.Cat(4,4,
                             Rope.Cat(3,3, 
                                      Rope.Cat(2,2, Rope.Str "x", Rope.Str "a"),
                                      Rope.Str "b"),
                             Rope.Str "c"),
                    Rope.Cat(2, 2, Rope.Str "d", Rope.Str "e"))));
      (None,
       (Rope.Str "x", Rope.Cat(3, 4, 
                        Rope.Cat(2, 2,Rope.Str "a", Rope.Str "b"), Rope.Cat(2, 2,Rope.Str "c", Rope.Str "d"))
                        ),
       Ok (Rope.Cat(4, 5, Rope.Cat(3,3,Rope.Cat(2,2,Rope.Str "x", Rope.Str "a"),Rope.Str "b"), Rope.Cat(2,2,Rope.Str "c", Rope.Str "d"))));
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
     (* Test Rope Cat*)
     (None, (Rope.Cat (1, 2, Rope.Str "a", Rope.Str "b"), 0, 1),
      Ok(Rope.Str "a"));
     (None, (Rope.Cat (1, 2, Rope.Str "a", Rope.Str "b"), 1, 1),
      Ok(Rope.Str "b"));
     (None, (Rope.Cat (1, 2, Rope.Str "a", Rope.Str "b"), 0, 2),
      Ok(Rope.Cat(1, 2, Rope.Str "a", Rope.Str "b")));
     (None, (Rope.Cat (2, 5, Rope.Cat(1,4, Rope.Str "ab", Rope.Str "cd"), Rope.Str "e"), 0, 2),
      Ok(Rope.Str "ab"));
     (None, (Rope.Cat (2, 5, Rope.Cat(1,4, Rope.Str "ab", Rope.Str "cd"), Rope.Str "e"), 10, 2),
      Error(Invalid_argument "index out of bounds"));
     (None, (Rope.Cat (2, 5, Rope.Cat(1, 4, Rope.Str "ab", Rope.Str "cd"), Rope.Str "e"), -1, 1),
      Error(Invalid_argument "index out of bounds"));
     (None, (Rope.Cat (2, 5, Rope.Cat(1, 4, Rope.Str "ab", Rope.Str "cd"), Rope.Str "e"), 0, 4),
      Ok(Rope.Cat (1,4, Rope.Str "ab", Rope.Str "cd")));
     (None, (Rope.Cat (2, 5, Rope.Cat(1, 4, Rope.Str "ab", Rope.Str "cd"), Rope.Str "e"), 1, 4),
      Ok(Rope.Cat (2, 4, Rope.Cat(1,3, Rope.Str "b", Rope.Str "cd"), Rope.Str "e")));
     (None, (Rope.Cat (2, 5, Rope.Cat(1, 4, Rope.Str "ab", Rope.Str "cd"), Rope.Str "e"), 2, 3),
      Ok(Rope.Cat (1, 3,  Rope.Str "cd", Rope.Str "e")));
     (* Right sided rope*)
     (None, (Rope.Cat (2, 5, Rope.Str("a"), Rope.Cat(1,4, Rope.Str("bc"),Rope.Str("de"))), 1, 4),
      Ok(Rope.Cat(1,4, Rope.Str("bc"), Rope.Str("de"))));
     (None, (Rope.Cat (2, 5, Rope.Str("a"), Rope.Cat(1, 4, Rope.Str("bc"),Rope.Str("de"))), 0, 4),
      Ok(Rope.Cat(2,4, Rope.Str("a"), Rope.Cat(1, 3, Rope.Str("bc"), Rope.Str("d")))));
   ]
  )
