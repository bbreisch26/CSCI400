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
      (
        fun stack cmd ->
          match (stack, cmd) with
          | (_, Num n) -> n::stack
          | ([], Op o) -> invalid_arg "rpn"
          | (first::[], Op o) -> invalid_arg "rpn"
          | (first::second::rest, Op Add) -> second + first :: rest
          | (first::second::rest, Op Sub) -> second - first :: rest
          | (first::second::rest, Op Mul) -> second * first :: rest
          | (first::second::rest, Op Div) -> second / first :: rest
      )
      []
      cmds

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
       (* DONE: Replace 0 *)
       match op with
       | Add -> eval (e1) + eval (e2)
       | Sub -> eval (e1) - eval (e2)
       | Mul -> eval (e1) * eval (e2)
       | Div -> eval (e1) / eval (e2)

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
     (Some "RPN simple", [RPN.Num 1; RPN.Num 2; RPN.Op Add], Ok [3]);
     (Some "RPN one arg", [RPN.Num 1; RPN.Op Add], Error (Invalid_argument "rpn"));
     (Some "RPN Commutativity", [RPN.Num 1; RPN.Num 2; RPN.Op Sub], Ok [-1]);
     (Some "RPN sub and add", [RPN.Num 1; RPN.Num 2; RPN.Num 3; RPN.Op Sub; RPN.Num 4;  RPN.Op Add], Ok [3; 1]);
     (* TODO *)
     (Some "RPN div by zero", [RPN.Num 1; RPN.Num 0; RPN.Op Div], Error (Division_by_zero));
     (Some "RPN no args", [RPN.Op Div], Error (Invalid_argument "rpn"));
     (Some "RPN add extra args", [RPN.Num 1; RPN.Num 2; RPN.Num 3; RPN.Num 4; RPN.Op Add], Ok [7; 2; 1]);
     (Some "RPN float result (round down)", [RPN.Num 5; RPN.Num 2; RPN.Op Div], Ok [2]);
     (Some "RPN mul extra args", [RPN.Num 1; RPN.Num 2; RPN.Num 10; RPN.Op Mul], Ok [20; 1]);
     (Some "RPN div extra args", [RPN.Num 1; RPN.Num 40; RPN.Num 10; RPN.Op Div], Ok [4; 1]);
     (Some "RPN extra ops", [RPN.Num 1; RPN.Num 4; RPN.Op Add; RPN.Op Div], Error (Invalid_argument "rpn"));
     (Some "RPN all ops", [RPN.Num 1; RPN.Num 2; RPN.Op Add; RPN.Num 2; RPN.Op Sub; RPN.Num 10; RPN.Op Mul; RPN.Num 5; RPN.Op Div; RPN.Num 1], Ok [1; 2]);
   ])


let exp_eval_tests =
  ("Exp.eval",
   Exp.eval, (=), (=),
   Some (Exp.string, string_of_int),
   [
     (Some "Simple Exp.Num", Exp.Num 1, Ok 1);
     (Some "Simple Exp", BinExp(Add, Exp.Num 1, Exp.Num 2), Ok 3);
     (Some "Exp Commutativity",  BinExp(Sub, Exp.Num 1, Exp.Num 2), Ok (-1));
     (* DONE *)
     (Some "Simple Exp with Op Mul", BinExp(Mul, Exp.Num 5, Exp.Num 6), Ok 30);
     (Some "Simple Exp with Op Div", BinExp(Div, Exp.Num 30, Exp.Num 6), Ok 5);
     (Some "Division by 0", BinExp(Div, Exp.Num 10, Exp.Num 0), Error (Division_by_zero));
     (Some "Multiplication of large numbers", BinExp(Mul, Exp.Num 413, Exp.Num 768), Ok 317184);
     (Some "Mutiplication by 0", BinExp(Mul, Exp.Num 7927, Exp.Num 0), Ok 0);
   ])
