open Util (* see util.ml *)

(*****************************)
(* PART II: Red-Black Trees *)
(*****************************)

module RBTree = struct
  type color = Red | Black
  type 'v tree =
    | Empty
    | Rnode of 'v tree * 'v * 'v tree (* Red node *)
    | Bnode of 'v tree * 'v * 'v tree (* Black node *)

  (* Return the color of an rbtree node *)
  let color t =
    match t with
      Rnode(_,_,_) -> Red
    | Bnode(_,_,_) | Empty -> Black

  (* Result of comparing two values *)
  (* if a < b, then cmp a b -> Lesser *)
  (* if a = b, then cmp a b -> Equal *)
  (* if a > b, then cmp a b -> Greater *)
  type cmp_result =
    | Lesser | Equal | Greater

  (* compare two data elements of type 'v *)
  type 'v cmp_fun = 'v -> 'v -> cmp_result

  (* Test if t satisfies the red-black tree invariants.
   *
   *  1. Does every red node have only black children?
   *  2. Does every path from root to leaf have the same number of black nodes?
   *)
  let is_invariant (t : 'v tree) : bool =
    (* TODO, remove false *)
    false

  (* Test if red-black tree t is sorted. *)
  let rec is_sorted (cmp : 'v cmp_fun) (t : 'v tree) : bool =
    (* TODO, remove false *)
    false

  (* Search for element x in red-black tree t.
   *
   * Return true if the tree contains x and false if it does not. *)
  let rec search (cmp : 'v cmp_fun) (t : 'v tree) (x:'v) : bool =
    (* TODO, remove false *)
    false

  (* Balance constructor for a red-black tree *)
  let balance (c:color) (l : 'v tree) (v : 'v ) (r : 'v tree) : 'v tree =
    match (c,l,v,r) with
    (* TODO, remove/modify the two cases below *)
    | (Red,_,_,_) -> Rnode(l,v,r)
    | (Black,_,_,_) -> Bnode(l,v,r)

  (* Insert element x into a red-black tree
   *
   * Do not reinsert (duplicate) existing elements *)
  let insert (cmp : 'v cmp_fun) (t : 'v tree) (x:'v) : 'v tree =
    (* TODO, remove t *)
    t


  (* Apply function f to every data element of tree t and collect the
   results in a list following an inorder traversal of the tree *)
  let map_inorder (f : 'v -> 'a) (t : 'v tree) : 'a list =
    (* TODO, remove [] *)
    []


  (* Apply function f to every data element of tree t and collect the
   results in a list following the reverse of an inorder traversal of the tree *)
  let map_revorder (f : 'v -> 'a) (t : 'v tree) : 'a list =
    (* TODO, remove [] *)
    []

end

(*********)
(* Tests *)
(*********)

(* See description in testing.ml *)

let int_cmp : int->int->RBTree.cmp_result =
  fun a b ->
  if a < b then RBTree.Lesser
  else if a > b then RBTree.Greater
  else RBTree.Equal

let str_cmp : string->string->RBTree.cmp_result =
  fun a b ->
  let c = compare a b in
  if c < 0 then RBTree.Lesser
  else if c > 0 then RBTree.Greater
  else RBTree.Equal

(* Convert tree to string *)
let rec str_x_rbtree f t =
  let h s l d r =
    Printf.sprintf "%s(%s,%s,%s)"
      s (str_x_rbtree f l) (f d) (str_x_rbtree f r)
  in
  match t with
  | RBTree.Empty -> "Empty"
  | RBTree.Rnode(l,d,r) ->
     h "Rnode" l d r
  | RBTree.Bnode(l,d,r) ->
     h "Bnode" l d r

let str_int_rbtree t = str_x_rbtree string_of_int t
let str_str_rbtree t = str_x_rbtree str_str t

let tree_arg_printer f =
  (fun (t,x) -> str_pair (str_x_rbtree f) f (t,x))

let int_tree_arg_printer = tree_arg_printer string_of_int
let str_tree_arg_printer = tree_arg_printer str_str

exception InvalidRBTreeError

(* To check that test case inputs are valid, sorted red-black trees *)
let check_rbtree (cmp : 'v RBTree.cmp_fun) (t: 'v RBTree.tree) =
  if (RBTree.is_invariant t) && (RBTree.is_sorted cmp t) then t
  else raise InvalidRBTreeError

(* To check that test case expected outputs are valid, sorted red-black trees *)
let eq_rbtree cmp_fun t1 t_expected =
  t1 = (check_rbtree cmp_fun t_expected)

(* Leaf node constants to make test inputs more readable *)
let r0 =  RBTree.Rnode(RBTree.Empty, 0,RBTree.Empty)
let r1 =  RBTree.Rnode(RBTree.Empty, 1,RBTree.Empty)
let r2 =  RBTree.Rnode(RBTree.Empty, 2,RBTree.Empty)
let r3 =  RBTree.Rnode(RBTree.Empty, 3,RBTree.Empty)
let r4 =  RBTree.Rnode(RBTree.Empty, 4,RBTree.Empty)
let r5 =  RBTree.Rnode(RBTree.Empty, 5,RBTree.Empty)
let r6 =  RBTree.Rnode(RBTree.Empty, 6,RBTree.Empty)
let r7 =  RBTree.Rnode(RBTree.Empty, 7,RBTree.Empty)
let r8 =  RBTree.Rnode(RBTree.Empty, 8,RBTree.Empty)
let r9 =  RBTree.Rnode(RBTree.Empty, 9,RBTree.Empty)
let r10 = RBTree.Rnode(RBTree.Empty,10,RBTree.Empty)
let r11 = RBTree.Rnode(RBTree.Empty,11,RBTree.Empty)
let r12 = RBTree.Rnode(RBTree.Empty,12,RBTree.Empty)
let r13 = RBTree.Rnode(RBTree.Empty,13,RBTree.Empty)
let r14 = RBTree.Rnode(RBTree.Empty,14,RBTree.Empty)
let r15 = RBTree.Rnode(RBTree.Empty,15,RBTree.Empty)
let r16 = RBTree.Rnode(RBTree.Empty,16,RBTree.Empty)
let r17 = RBTree.Rnode(RBTree.Empty,17,RBTree.Empty)
let r18 = RBTree.Rnode(RBTree.Empty,18,RBTree.Empty)
let r19 = RBTree.Rnode(RBTree.Empty,19,RBTree.Empty)
let r20 = RBTree.Rnode(RBTree.Empty,20,RBTree.Empty)
let r21 = RBTree.Rnode(RBTree.Empty,21,RBTree.Empty)
let r22 = RBTree.Rnode(RBTree.Empty,22,RBTree.Empty)
let r23 = RBTree.Rnode(RBTree.Empty,23,RBTree.Empty)
let r24 = RBTree.Rnode(RBTree.Empty,24,RBTree.Empty)
let r25 = RBTree.Rnode(RBTree.Empty,25,RBTree.Empty)
let r26 = RBTree.Rnode(RBTree.Empty,26,RBTree.Empty)
let r27 = RBTree.Rnode(RBTree.Empty,27,RBTree.Empty)
let r28 = RBTree.Rnode(RBTree.Empty,28,RBTree.Empty)
let r29 = RBTree.Rnode(RBTree.Empty,29,RBTree.Empty)
let r30 = RBTree.Rnode(RBTree.Empty,30,RBTree.Empty)
let r31 = RBTree.Rnode(RBTree.Empty,31,RBTree.Empty)
let r32 = RBTree.Rnode(RBTree.Empty,32,RBTree.Empty)
let r33 = RBTree.Rnode(RBTree.Empty,33,RBTree.Empty)
let r34 = RBTree.Rnode(RBTree.Empty,34,RBTree.Empty)

let b0 =  RBTree.Bnode(RBTree.Empty, 0,RBTree.Empty)
let b1 =  RBTree.Bnode(RBTree.Empty, 1,RBTree.Empty)
let b2 =  RBTree.Bnode(RBTree.Empty, 2,RBTree.Empty)
let b3 =  RBTree.Bnode(RBTree.Empty, 3,RBTree.Empty)
let b4 =  RBTree.Bnode(RBTree.Empty, 4,RBTree.Empty)
let b5 =  RBTree.Bnode(RBTree.Empty, 5,RBTree.Empty)
let b6 =  RBTree.Bnode(RBTree.Empty, 6,RBTree.Empty)
let b7 =  RBTree.Bnode(RBTree.Empty, 7,RBTree.Empty)
let b8 =  RBTree.Bnode(RBTree.Empty, 8,RBTree.Empty)
let b9 =  RBTree.Bnode(RBTree.Empty, 9,RBTree.Empty)
let b10 = RBTree.Bnode(RBTree.Empty,10,RBTree.Empty)
let b11 = RBTree.Bnode(RBTree.Empty,11,RBTree.Empty)
let b12 = RBTree.Bnode(RBTree.Empty,12,RBTree.Empty)
let b13 = RBTree.Bnode(RBTree.Empty,13,RBTree.Empty)
let b14 = RBTree.Bnode(RBTree.Empty,14,RBTree.Empty)
let b15 = RBTree.Bnode(RBTree.Empty,15,RBTree.Empty)
let b16 = RBTree.Bnode(RBTree.Empty,16,RBTree.Empty)
let b17 = RBTree.Bnode(RBTree.Empty,17,RBTree.Empty)
let b18 = RBTree.Bnode(RBTree.Empty,18,RBTree.Empty)
let b19 = RBTree.Bnode(RBTree.Empty,19,RBTree.Empty)
let b20 = RBTree.Bnode(RBTree.Empty,20,RBTree.Empty)

let ra =  RBTree.Rnode(RBTree.Empty,"a",RBTree.Empty)
let rb =  RBTree.Rnode(RBTree.Empty,"b",RBTree.Empty)
let rc =  RBTree.Rnode(RBTree.Empty,"c",RBTree.Empty)
let rd =  RBTree.Rnode(RBTree.Empty,"d",RBTree.Empty)

let ba =  RBTree.Bnode(RBTree.Empty,"a",RBTree.Empty)
let bb =  RBTree.Bnode(RBTree.Empty,"b",RBTree.Empty)
let bc =  RBTree.Bnode(RBTree.Empty,"c",RBTree.Empty)
let bd =  RBTree.Bnode(RBTree.Empty,"d",RBTree.Empty)

let rbt_is_invariant_int_tests =
  ("rbt_is_invariant_int",
   RBTree.is_invariant,
   (=), (=),
   Some(str_int_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      RBTree.Bnode(r1, 2, r3),
      Ok(true));
     (* TODO *)
   ])

let rbt_is_invariant_str_tests =
  ("rbt_is_invariant_str",
   RBTree.is_invariant,
   (=), (=),
   Some(str_str_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      RBTree.Bnode(ra, "b", rc),
      Ok(true));
     (* TODO *)
   ])

let rbt_is_sorted_int_tests =
  ("rbt_is_sorted_int",
   (fun t -> RBTree.is_sorted int_cmp t),
   (=), (=),
   Some(str_int_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      RBTree.Bnode(r1, 2, r3),
      Ok(true));
     (* TODO *)

   ])


let rbt_is_sorted_str_tests =
  ("rbt_is_sorted_str",
   (fun t -> RBTree.is_sorted str_cmp t),
   (=), (=),
   Some(str_str_rbtree,
        str_bool),
   [
     (Some("simple tree"),
      RBTree.Bnode(ra, "b", rc),
      Ok(true));
     (* TODO *)
   ])

let rbt_search_int_tests =
  ("rbt_search_int",
   (fun (t,x) -> RBTree.search int_cmp (check_rbtree int_cmp t) x),
   (=), (=),
   Some(int_tree_arg_printer, str_bool),
   [
     (Some("simple tree"),
      (RBTree.Bnode(r1, 2, r3), 2),
      Ok(true));
     (* TODO *)
   ])

let rbt_search_str_tests =
  ("rbt_search_str",
   (fun (t,x) -> RBTree.search str_cmp (check_rbtree str_cmp t) x),
   (=), (=),
   Some(str_tree_arg_printer, str_bool),
   [
     (Some("simple tree"),
      (RBTree.Bnode(ra, "b", rc), "b"),
      Ok(true));
     (* TODO *)
   ])

let rbt_balance_tester t =
  (* Note: RBTree.balance does not always return a balanced tree!  We
     only enforce balance when we reach the black grandparent in a
     red-red invariant violation. *)
  match t with
    RBTree.Empty -> raise InvalidRBTreeError (* we don't ever balance empty trees *)
  | RBTree.Rnode(l,v,r) | RBTree.Bnode(l,v,r)
    -> RBTree.balance (RBTree.color t) l v r

let rbt_balance_int_tests =
  ("rbt_balance_int",
   rbt_balance_tester,
   (=), (=),
   Some(str_int_rbtree, str_int_rbtree),
   [
     (Some("Case A"),
      RBTree.Bnode(RBTree.Rnode(r1,2,RBTree.Empty),
            3,
            RBTree.Empty),
      Ok(RBTree.Rnode(b1,2,b3)));
     (* TODO *)


   ])

let rbt_balance_str_tests =
  ("rbt_balance_str",
   rbt_balance_tester,
   (=), (=),
   Some(str_str_rbtree, str_str_rbtree),
   [
     (Some("Case A"),
      RBTree.Bnode(RBTree.Rnode(ra,"b",RBTree.Empty),
            "c",
            RBTree.Empty),
      Ok(RBTree.Rnode(ba,"b",bc)))
       (* TODO *)
   ])

let rbt_insert_tester f =
  fun (t,x) ->
  check_rbtree f
    (RBTree.insert f
       (check_rbtree f t)
       x)

let int_rbt_insert_tester = rbt_insert_tester int_cmp
let int_rbt_insert_tests_eq = eq_rbtree int_cmp
let rbt_insert_int_tests =
  ("rbt_insert_int",
   int_rbt_insert_tester,
   int_rbt_insert_tests_eq,  (=),
   Some(int_tree_arg_printer,
        str_int_rbtree),
   [
     (Some("simple tree"),
      (RBTree.Bnode(r1, 2, RBTree.Empty), 3),
      Ok(RBTree.Bnode(r1, 2, r3)));
     (* TODO *)
   ])

let str_rbt_insert_tester = rbt_insert_tester str_cmp
let str_rbt_insert_tests_eq = eq_rbtree str_cmp
let rbt_insert_str_tests =
  ("rbt_insert_str",
   str_rbt_insert_tester,
   str_rbt_insert_tests_eq,  (=),
   Some(str_tree_arg_printer,
        str_str_rbtree),
   [
     (Some("simple tree"),
      (RBTree.Bnode(ra, "b", RBTree.Empty), "c"),
      Ok(RBTree.Bnode(ra, "b", rc)));
     (* TODO *)
   ])


let map_printer =
  Some((fun (f,t) -> str_int_rbtree t),
       str_int_list)


let identity x = x

let rbt_map_inorder_tests =
  ("rbt_map_inorder",
   (fun (f,t) -> RBTree.map_inorder f t),
   (=), (=),
   map_printer,
   [
     (Some("simple tree"),
      (identity,RBTree.Bnode(r1, 2, r3)),
      Ok([1; 2; 3]));
     (* TODO *)
  ])

let rbt_map_revorder_tests =
  ("rbt_map_inorder",
   (fun (f,t) -> RBTree.map_revorder f t),
   (=), (=),
   map_printer,
   [
     (Some("simple tree"),
      (identity,RBTree.Bnode(r1, 2, r3)),
      Ok([3; 2; 1]));
     (* TODO *)
  ])
