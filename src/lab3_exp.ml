open Util (* see util.ml *)

(********************************)
(* PART I: Arithmetic Evaluator *)
(********************************)

type bop = Add | Sub | Mul | Div

(* Reverse Polish Notation (RPN) *)
module RPN = struct
  type cmd =
    | Op of bop
    | Num of int

  type exp = cmd list

  (* RPN evaluator.  Return the stack after evaluating commands. *)
  let eval (cmds : exp) : int list =
    List.fold_left
      (fun stack cmd ->
        (* TODO: replace [] *)
        [])
      [] cmds

  let string cmds =
    "[" ^ str_x_list (fun cmd -> match cmd with
                                 | Op Add -> "+"
                                 | Op Sub -> "-"
                                 | Op Mul -> "*"
                                 | Op Div -> "/"
                                 | Num x -> string_of_int x)
            cmds ";" ^ "]"

end



(* Expression Trees *)
module Exp = struct
  type exp =
    | BinExp of bop * exp * exp
    | Num of int

  (* Evaluate the expression *)
  let rec eval (e : exp) : int =
    match e with
    | Num x -> x
    | BinExp(op,e1,e2) ->
       (* TODO: Replace 0 *)
       0

  (* Convert the expression to a string.  The string must be fully
   parenthesized and contain no whitespace. *)
  let rec string (e : exp) : string =
    match e with
    | Num x -> string_of_int x
    | BinExp(op,e1,e2) ->
       "("
       ^ (string e1)
       ^ (match op with
          | Add -> "+"
          | Sub -> "-"
          | Mul -> "*"
          | Div -> "/")
       ^ (string e2)
       ^ ")"

  (* Convert the infix expression to the equivalent rpn expression. *)
  let rec rpn (e : exp) : RPN.exp =
    match e with
    | Num x -> [RPN.Num x]
    | BinExp(op,e1,e2) ->
       (rpn e1)
       @ (rpn e2)
       @ [RPN.Op op]

end

(*********)
(* Tests *)
(*********)

let exp_string_tests =
  ("Exp.string",
   Exp.string, (=), (=),
   Some (Exp.string, fun x->x),
   [
     (Some "Simple Num", Exp.Num 1, Ok "1");
     (Some "Simple Exp", Exp.BinExp(Add, Exp.Num 1, Exp.Num 2), Ok "(1+2)");
     (Some "Exp Commutativity",  Exp.BinExp(Sub, Exp.Num 1, Exp.Num 2), Ok "(1-2)");
   ])


let exp_rpn_tests =
  ("Exp.rpn",
   Exp.rpn, (=), (=),
   Some (Exp.string, RPN.string),
   [
     (Some "Simple Exp.Num", Exp.Num 1, Ok [RPN.Num 1]);
     (Some "Simple Exp", BinExp(Add, Exp.Num 1, Exp.Num 2), Ok [RPN.Num 1; RPN.Num 2; RPN.Op Add]);
     (Some "Another Exp", BinExp(Mul, BinExp(Add, Exp.Num 1, Exp.Num 2), Exp.Num 3),
      Ok [RPN.Num 1; RPN.Num 2; RPN.Op Add; RPN.Num 3; RPN.Op Mul]);
   ])


let rpn_eval_tests =
  ("RPN.eval",
   RPN.eval, (=), (=),
   Some(RPN.string, str_int_list),
   [
     (Some "Simple RPN", [RPN.Num 1; RPN.Num 2; RPN.Op Add], Ok [3]);
     (Some "RPN Inval", [RPN.Num 1; RPN.Op Add], Error (Invalid_argument "rpn"));
     (Some "RPN Commutativity", [RPN.Num 1; RPN.Num 2;  RPN.Op Sub], Ok [-1]);
     (Some "More RPN", [RPN.Num 1; RPN.Num 2; RPN.Num 3;  RPN.Op Sub; RPN.Num 4;  RPN.Op Add], Ok [3; 1]);
     (* TODO *)
   ])


let exp_eval_tests =
  ("Exp.eval",
   Exp.eval, (=), (=),
   Some (Exp.string, string_of_int),
   [
     (Some "Simple Exp.Num", Exp.Num 1, Ok 1);
     (Some "Simple Exp", BinExp(Add, Exp.Num 1, Exp.Num 2), Ok 3);
     (Some "Exp Commutativity",  BinExp(Sub, Exp.Num 1, Exp.Num 2), Ok (-1));
     (* TODO *)
   ])
