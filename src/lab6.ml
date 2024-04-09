open Javascript_ast
open Javascript_main
open Util

(*
 * Check javascript_ast.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 *)

(* basic tests to show how the value conversion functions work (do not modify) *)
let simple_to_num_tests =
  ("Simple ToNum Conversions", to_num, (fun n1 n2 -> eq_float (n1,n2)), eq_exn, Some(str_value,str_float),
   [
     (None, NumVal(123.0), Ok(123.0));
     (None, BoolVal(true), Ok(1.0));
     (None, StrVal(""),    Ok(0.0));
  ])
let simple_to_bool_tests =
  ("Simple ToBool Conversions", to_bool, (=), eq_exn, Some(str_value,string_of_bool),
   [
     (None, BoolVal(true),  Ok(true));
     (None, NumVal(1.0),    Ok(true));
     (None, StrVal("true"), Ok(true));
  ])
let simple_to_str_tests =
  ("Simple ToStr Conversions", to_str, (=), eq_exn, Some(str_value,(fun x -> x)),
   [
     (None, StrVal("hello"), Ok("hello"));
     (None, BoolVal(true),   Ok("true"));
     (None, NumVal(1.234),   Ok("1.234"));
     (None, NumVal(1.000),   Ok("1"));
     (None, NumVal(0.00),    Ok("0"));
     (None, NumVal(100.01),  Ok("100.01"));
  ])

(* (eval p) should reduce a program to a *value* (if Node.js produces
 * a *value* for an example JavaScript program, your evaluator should
 * produce that same value).  *)

(* evaluate a program *)
let rec eval (p : program_t) : value_t = match p with
  | ExprProgram(_,e) -> eval_expr e
  | _ -> raise (UnimplementedProgram(p))


(* evaluate a value *)
and eval_expr (e:expr_t) : value_t =  match e with
  | ValExpr(p,v) -> v
  (* MinusBop provided as an example *)
  | BopExpr(_,e1,MinusBop,e2) ->
     NumVal(to_num (eval_expr e1) -. to_num (eval_expr e2))
  (* TODO *)
  (* Unary Operators *)
  | UopExpr(_,NotUop,e1) ->
     BoolVal(not (to_bool (eval_expr e1)))
  | UopExpr(_,NegUop,e1) ->
     NumVal(-. (to_num (eval_expr e1)))
  | UopExpr(_,PosUop,e1) ->
     NumVal(+. (to_num (eval_expr e1)))
  (* Binary operators *)
  | BopExpr(_,e1,PlusBop,e2) ->
     NumVal(to_num (eval_expr e1) +. to_num (eval_expr e2))
  | BopExpr(_,e1,TimesBop,e2) ->
     NumVal(to_num (eval_expr e1) *. to_num (eval_expr e2))
  | BopExpr(_,e1,DivBop,e2) ->
     NumVal(to_num (eval_expr e1) /. to_num (eval_expr e2))
  (* Boolean expressions *)
  | BopExpr(_,e1,EqBop,e2) ->
     BoolVal(to_num (eval_expr e1) = to_num (eval_expr e2))
  | BopExpr(_,e1,NeqBop,e2) ->
     BoolVal(to_num (eval_expr e1) <> to_num (eval_expr e2))
  | BopExpr(_,e1,LtBop,e2) ->
     BoolVal(to_num (eval_expr e1) < to_num (eval_expr e2))
  | BopExpr(_,e1,LteBop,e2) ->
     BoolVal(to_num (eval_expr e1) <= to_num (eval_expr e2))
  | BopExpr(_,e1,GtBop,e2) ->
     BoolVal(to_num (eval_expr e1) > to_num (eval_expr e2))
  | BopExpr(_,e1,GteBop,e2) ->
     BoolVal(to_num (eval_expr e1) >= to_num (eval_expr e2))
  (* Need to expand this to choose between bool && and float/int && *)
  | BopExpr(_,e1,AndBop,e2) ->
     BoolVal(to_bool (eval_expr e1) && to_bool (eval_expr e2))
  (* Task 2: console.log *)
  | PrintExpr(_,e) ->
     print_string ( to_str (eval_expr e));
     UndefVal
  (* Task 3: conditional *)
  | IfExpr(_,e1,e2,e3) ->
     if (to_bool (eval_expr e1)) then
       NumVal(to_num (eval_expr e2))
     else
       NumVal(to_num (eval_expr e3))
     
  (* other expression types unimplemented *)
  | _ -> raise (UnimplementedExpr(e))


(*********)
(* Tests *)
(*********)

let test_group name tests =
  (name, compose eval parse_string, eq_value, eq_exn,
   Some((fun (x:string) -> x),str_value), tests)


(* basic tests for the evaluator (do not modify) *)
let simple_expr_eval_tests =
  test_group "Simple Expression Evaluation"
    [
      (None, "1 + true",                    Ok(NumVal(2.0)));
      (None, "false + true",                Ok(NumVal(1.0)));
      (None, "100 || 200",                  Ok(NumVal(100.0)));
      (None, "-false",                      Ok(NumVal(0.0)));
      (None, "1 + 1",                       Ok(NumVal(2.0)));
      (None, "3 + (4 + 5)",                 Ok(NumVal(12.0)));
      (None, "3 * (4 + 5)",                 Ok(NumVal(27.0)));
      (None, "-6 * 90 - 8",                 Ok(NumVal(-548.0)));
      (None, "-100 + 50",                   Ok(NumVal(-50.0)));
      (None, "true && (false || true)",     Ok(BoolVal(true)));
      (None, "true && (false || !true)",    Ok(BoolVal(false)));
      (None, "1 < 2",                       Ok(BoolVal(true)));
      (None, "100 === 100",                 Ok(BoolVal(true)));
      (None, "100 === 101",                 Ok(BoolVal(false)));
      (None, "100 !== 200",                 Ok(BoolVal(true)));
      (None, "true === true",               Ok(BoolVal(true)));
      (None, "0 / 0",                       Ok(NumVal(nan)));
    ]


let simple_print_eval_tests =
  test_group "Simple Print Evaluation"
    [
      (None, "console.log(\"Hello World\")",           Ok(UndefVal));
    ]

let simple_cond_eval_tests =
  test_group "Simple Conditional Evaluation"
    [
      (None, "(1 < 2) ? 123 : 124",         Ok(NumVal(123.0)));
    ]

let simple_str_eval_tests =
  test_group "Simple String Evaluation"
    [
      (None, "\"aaa\" < \"aaaa\"",          Ok(BoolVal(true)));
      (None, "\"bbb\" < \"aaa\"",           Ok(BoolVal(false)));
      (None, "\"hello\"+\" \"+\"world\"",   Ok(StrVal("hello world")));
    ]

let eval_tests =
  test_group "Evaluator"
    [
      (* TODO *)
    ]

let cond_eval_tests =
  test_group "Conditional Evaluation"
    [
      (* TODO *)
    ]

let str_eval_tests =
  test_group "String Evaluation"
    [
      (* TODO *)
    ]
