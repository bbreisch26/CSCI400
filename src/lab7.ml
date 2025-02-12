open Javascript_ast
open Javascript_env
open Javascript_main
open Util

(*
 * Check javascript_ast.ml and javascript_env.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 * - bind_environment        -- add a variable binding to the environment
 * - read_environment        -- look up a variable's value in the environment
 * - empty_env               -- the empty environment
 *)

(*
 * (eval env p) should reduce a program in initial environment env to a *value*
 * In general, if Node.js produces a *value* for an example JavaScript
 * program, your evaluator should produce that same value.
 * You should support basic JavaScript (recursive higher-order) functions
 * with lexical scoping.
 *
 * See the assignment README for more details.
 *)

(* evaluate a program *)
let rec eval (env : environment_t) (p: program_t) : value_t = match p with
  | ExprProgram(_,e) -> eval_expr env e
  (* DONE *)
  | StmtProgram(_,s,p1) -> (* Bind statement to environment and evaluate e *)
     eval (eval_stmt env s) p1
(* | _ -> raise (UnimplementedProgram(p)) (not needed because we cover every possible value of p*)

(* evaluate a block *)
and eval_block (env:environment_t) (p:block_t) : value_t = match p with
  | ReturnBlock(_,e) -> eval_expr env e
  (* DONE *)
  | StmtBlock(_,s,b) -> eval_block (eval_stmt env s) b

(* evaluate a statement *)
and eval_stmt (env:environment_t) (s:stmt_t) : environment_t = match s with
  (* DONE *) (* Ensure Mutable/Immutable is correct *)
  | ConstStmt(_,id, e1) ->
     bind_environment env id Immutable (eval_expr env e1)
 (* | LetStmt(_,id,e1) ->
     bind_environment env id Mutable (eval_expr env e1)
  | AssignStmt(_,e1,e2) ->
     bind_environment env (to_str (eval_expr env e1)) Mutable (eval_expr env e2) *)
  | _ -> raise (UnimplementedStmt(s))

(* Binds arguments to env based on arguments and their corresponding expressions *)
(* Expressions are evaluated using the outer environment *)
and bind_args (env:environment_t) (defenv: environment_t) (names: typed_ident_t list) (exps: expr_t list) =
  match (names, exps) with
  | (((fn,_)::rn),(efirst::erest)) -> bind_args (bind_environment env fn Immutable (eval_expr defenv efirst)) defenv rn erest
  | _ -> env
(* evaluate a value *)
and eval_expr (env:environment_t) (e:expr_t) : value_t =
  (* Printf.printf "%s %s\n" (str_expr e) (str_environment_simple env); *)
  match e with
  | BlockExpr(p,b) -> eval_block env b
  | BopExpr(_,e1,MinusBop,e2) ->
     NumVal(to_num (eval_expr env e1) -. to_num (eval_expr env e2))
  (* TODO *)
  (* Variable Expression - look up var name in current environment *)
  | VarExpr(p, name) ->
    (match read_environment env name with
    | Some(_, value) -> value
    | None -> raise (UndeclaredVar(name))
    )
  | ValExpr(p,v) -> v
  (* and lambda_t = (ident_t option * typed_ident_t list * block_t * typ_t option)*)
  (* Function expression - don't bind to environment (yet)*)
  | FuncExpr(p,l) ->
    (match l with
    | (Some(name), params, block, None) -> ClosureVal(env, l) (*Do something special with named function def*)
    | (_, params, block, _) -> ClosureVal(env, l)
    )
  (* Function call - bind a function (ClosureVal) to the environment if it is named *)
  | CallExpr(p, name, vals) ->
     let func = eval_expr env name in
     (match func with
      | ClosureVal(defEnv,l) ->
         (match (l) with
          | (Some(name), args, block, _) ->
             let funcEnv = (bind_environment defEnv name Immutable func) in
             (eval_block (bind_args funcEnv env args vals)  block)
          (* Anonymous function, no name to find to Env *)
          | (None, args, block, _) -> eval_block (bind_args defEnv env args vals) block
         )
      | _ -> raise(InvalidCall(name))
     )


  (* Lab 6 Code below w/ env added to any eval_expr *)
  (* Unary Operators *)
  | UopExpr(_,NotUop,e1) ->
    BoolVal(not (to_bool (eval_expr env e1)))
  | UopExpr(_,NegUop,e1) ->
      NumVal(-. (to_num (eval_expr env e1)))
  | UopExpr(_,PosUop,e1) ->
     NumVal(+. (to_num (eval_expr env e1)))
  (* Binary Operators *)
  | BopExpr(_,e1,PlusBop,e2) ->
    (let eval_e1 = eval_expr env e1 in
      let eval_e2 = eval_expr env e2 in
      match (eval_e1, eval_e2) with
      | (StrVal(_), _)
      | (_, StrVal(_)) -> StrVal(to_str eval_e1 ^ to_str eval_e2)
      | _ -> NumVal(to_num eval_e1 +. to_num eval_e2))
  | BopExpr(_,e1,TimesBop,e2) ->
      NumVal(to_num (eval_expr env e1) *. to_num (eval_expr env e2))
  | BopExpr(_,e1,DivBop,e2) ->
      NumVal(to_num (eval_expr env e1) /. to_num (eval_expr env e2))
  (* Boolean expressions *)
  (* Strict inequality - differing types always return false *)
  (* See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality *)
  | BopExpr(_,e1,EqBop,e2) ->
      (let eval_e1 = eval_expr env e1 in
      let eval_e2 = eval_expr env e2 in
      match (eval_e1, eval_e2) with
      | (StrVal(_), StrVal(_))
      | (NumVal(_), NumVal(_))
      | (BoolVal(_), BoolVal(_))
      | (UndefVal, UndefVal) -> BoolVal(eval_e1 = eval_e2)
      | _ -> BoolVal(false)) (* Types must be different in this case - false *)
  (* Strict inequality always returns true with differing types *)
  | BopExpr(_,e1,NeqBop,e2) ->
      (let eval_e1 = eval_expr env e1 in
      let eval_e2 = eval_expr env e2 in
      match (eval_e1, eval_e2) with
      | (StrVal(_), StrVal(_))
      | (NumVal(_), NumVal(_))
      | (BoolVal(_), BoolVal(_))
      | (UndefVal, UndefVal) -> BoolVal(eval_e1 <> eval_e2)
      | _ -> BoolVal(true)) (* Types must be different in this case - !false = true *)
  | BopExpr(_,e1,LtBop,e2) ->
      (let eval_e1 = eval_expr env e1 in
      let eval_e2 = eval_expr env e2 in
      match (eval_e1, eval_e2) with
      | (StrVal(_),StrVal(_)) -> BoolVal(eval_e1 < eval_e2) (* len comparison only used in JS when both operands are strings *)
      | _ -> BoolVal(to_num eval_e1 < to_num eval_e2))
  | BopExpr(_,e1,LteBop,e2) ->
    (let eval_e1 = eval_expr env e1 in
      let eval_e2 = eval_expr env e2 in
      match (eval_e1, eval_e2) with
      | (StrVal(_), StrVal(_)) -> BoolVal(eval_e1 <= eval_e2)
      | _ -> BoolVal(to_num eval_e1 <= to_num eval_e2))
  | BopExpr(_,e1,GtBop,e2) ->
    (let eval_e1 = eval_expr env e1 in
      let eval_e2 = eval_expr env e2 in
      match (eval_e1, eval_e2) with
      | (StrVal(_), StrVal(_)) -> BoolVal(eval_e1 > eval_e2)
      | _ -> BoolVal(to_num eval_e1 > to_num eval_e2))
    | BopExpr(_,e1,GteBop,e2) ->
    (let eval_e1 = eval_expr env e1 in
      let eval_e2 = eval_expr env e2 in
      match (eval_e1, eval_e2) with
      | (StrVal(_), StrVal(_)) -> BoolVal(eval_e1 >= eval_e2)
      | _ -> BoolVal(to_num eval_e1 >= to_num eval_e2))
  (* Have to handle weird javascript rules - nonzero numbers are true *)
  (* See slide 20 of L17-semantics-activity-postlecture.pdf for explanation*)
  | BopExpr(_,e1,AndBop,e2) ->
      (match (to_bool (eval_expr env e1), eval_expr env e2) with
      | (true, v2) ->  eval_expr env e2
      | (false, v2) -> eval_expr env e1)
  | BopExpr(_,e1,OrBop,e2) ->
      (match (to_bool (eval_expr env e1), eval_expr env e2) with
      | (true, v2) -> eval_expr env e1 
      | (false, v2) -> eval_expr env e2)
  | PrintExpr(_,e) ->
      print_endline ( to_str (eval_expr env e));
      UndefVal
  | IfExpr(_,e1,e2,e3) ->
      if (to_bool (eval_expr env e1)) then
        eval_expr env e2
      else
        eval_expr env e3
  
  (* other expression types unimplemented *)
  | _ -> raise (UnimplementedExpr(e))



(*********)
(* Tests *)
(*********)

let test_group name tests =
  (name, compose (eval empty_env) parse_string, eq_value, eq_exn,
   Some((fun (x : string) -> x),str_value),
   (* None, *)
   tests)

(* basic tests for the evaluator (do not modify) *)
let simple_expr_eval_tests =
  test_group "Simple Expression Evaluation"
    [
      (None, "1 + true",                     Ok(NumVal(2.0)));
      (None, "false + true",                 Ok(NumVal(1.0)));
      (None, "100 || 200",                   Ok(NumVal(100.0)));
      (None, "-false",                       Ok(NumVal(0.0)));
      (None, "1 + 1",                        Ok(NumVal(2.0)));
      (None, "3 + (4 + 5)",                  Ok(NumVal(12.0)));
      (None, "3 * (4 + 5)",                  Ok(NumVal(27.0)));
      (None, "-6 * 90 - 8",                  Ok(NumVal(-548.0)));
      (None, "-100 + 50",                    Ok(NumVal(-50.0)));
      (None, "true && (false || true)",      Ok(BoolVal(true)));
      (None, "true && (false || !true)",     Ok(BoolVal(false)));
      (None, "1 < 2",                        Ok(BoolVal(true)));
      (None, "100 === 100",                  Ok(BoolVal(true)));
      (None, "100 === 101",                  Ok(BoolVal(false)));
      (None, "100 !== 200",                  Ok(BoolVal(true)));
      (None, "true === true",                Ok(BoolVal(true)));
      (None, "0 / 0",                        Ok(NumVal(nan)));
      (None, "console.log(\"Hello World\")", Ok(UndefVal));
      (None, "(1 < 2) ? 123 : 124",          Ok(NumVal(123.0)));
      (None, "\"aaa\" < \"aaaa\"",           Ok(BoolVal(true)));
      (None, "\"bbb\" < \"aaa\"",            Ok(BoolVal(false)));
      (None, "\"hello\"+\" \"+\"world\"",    Ok(StrVal("hello world")));
    ]

let simple_var_eval_tests =
  test_group "Simple Variable Evaluation"
    [
      (None, "const x = 1; x+1",            Ok(NumVal(2.0)));
      (None, "const x=1; const y=2; x+y",   Ok(NumVal(3.0)));
      (None, "const x=3; const y=x*2+1; y", Ok(NumVal(7.0)));
      (None, "const x = 1; y",              Error(UndeclaredVar("y")));
    ]


let var_eval_tests =
  test_group "Variable Evaluation"
    [
      (None, "const x = true; x + 1", Ok(NumVal(2.0)));
      (* Despite const being immutable, we allow redefinition *)
      (None, "const x = 1; const x = 2; x + 1", Ok(NumVal(3.0)));
      (None, "const x = 0; const y = 10; x + y", Ok(NumVal(10.0)));
      (None, "const x = undefined; const y = undefined; x === y", Ok(BoolVal(true)));
      (None, "const x = 1; const y = 2; const z = 3; x + y + z", Ok(NumVal(6.0)));
      (None, "const x=1; const y=2; x + y", Ok(NumVal(3.0)));
    ]


let fact_js = "function factorial(n){return (n <= 1) ? 1 : (n * factorial(n-1));}"
let fib_js = "function fib(x){return x<=0 ? 0 : (x===1 ? 1 : fib(x-1)+fib(x-2));}"
let scopes_js =
"(function (x) {
    return function(f) {
        return function (x) {
            return f(0);
        }(2);
    } (function (y) {return x;});
} (1))"

let readme1_js =
  "const f = function(x){ return x+1; };
   const r = f(2);
   r+3"

let readme2_js =
  "const x = 5;
   const f = function(y){ return x + y; };
   (function(z) { const x = 7; return f(6); })(0)"

(* basic tests for the evaluator (do not modify) *)
let simple_func_eval_tests =
  test_group "Simple Function Definition Evaluation"
    [
      (None, "function test(x){const x = 123; return x;}",
       Ok(ClosureVal(StringMap.empty,(
                Some("test"),
                [("x",None)],
                StmtBlock(NoPos,
                          ConstStmt(NoPos,
                                    "x",
                                    ValExpr(NoPos,NumVal(123.0))),
                          ReturnBlock(NoPos,VarExpr(NoPos,"x"))),
                None))));
      (None, fact_js,
       Ok(ClosureVal(StringMap.empty,(
                Some("factorial"),
                [("n",None)],
                ReturnBlock(NoPos,IfExpr(NoPos,
                                         BopExpr(NoPos,VarExpr(NoPos,"n"),LteBop,ValExpr(NoPos,NumVal(1.0))),
                                         ValExpr(NoPos,NumVal(1.0)),
                                         BopExpr(NoPos,VarExpr(NoPos,"n"),TimesBop,CallExpr(NoPos,VarExpr(NoPos,"factorial"),[BopExpr(NoPos,VarExpr(NoPos,"n"),MinusBop,ValExpr(NoPos,NumVal(1.0)))]))
                  )),
                None))));

    ]
(* note - you can use the following to print a program for debugging *)
(* let _ = Printf.printf "RESULT = %s\n" (str_program (parse_string "const x = 1 + 1; x * 2")*)
let func_eval_tests =
  test_group "Function Definition Evaluation"
    [
      (* TODO *)
      (None, "function sum(x, y){return x + y;}",
       Ok(ClosureVal(StringMap.empty,(
                Some("sum"),
                [("x",None); ("y",None)],
                          ReturnBlock(NoPos,BopExpr(NoPos,VarExpr(NoPos,"x"),
                                      PlusBop,VarExpr(NoPos,"y"))),
                          None))));
      (Some("fib"), fib_js,
       Ok(ClosureVal(StringMap.empty,(
                Some("fib"),
                [("x",None)],
                          ReturnBlock(NoPos,IfExpr(NoPos, 
                            BopExpr(NoPos,VarExpr(NoPos,"x"), LteBop,ValExpr(NoPos,NumVal(0.0))),
                            ValExpr(NoPos,NumVal(0.0)),
                            IfExpr(NoPos,
                              BopExpr(NoPos,VarExpr(NoPos,"x"),EqBop,ValExpr(NoPos,NumVal(1.0))),
                              ValExpr(NoPos,NumVal(1.0)),
                              BopExpr(NoPos,
                                CallExpr(NoPos,VarExpr(NoPos,"fib"),[BopExpr(NoPos,VarExpr(NoPos,"x"),
                                         MinusBop,ValExpr(NoPos,NumVal(1.0)))]),
                                PlusBop,
                                CallExpr(NoPos,VarExpr(NoPos,"fib"),[BopExpr(NoPos,VarExpr(NoPos,"x"),
                                         MinusBop,ValExpr(NoPos,NumVal(2.0)))]))))),
  None))));(**)
      
    ]

let simple_call_eval_tests =
  test_group "Simple Call Evaluation"
    [
      (None, "const f = function(x){return x+1;}; f(1)",                    Ok(NumVal(2.0)));
      (None, "const y = 5; const f = function(x){return x+1;}; f(y)",       Ok(NumVal(6.0)));
      (Some("recursion"), "const f = function t(x){return x===0 ? 0 : x+t(x-1);}; f(5)", Ok(NumVal(15.0)));

      (Some("Readme 1"), readme1_js, Ok(NumVal(6.0)));
      (Some("Readme 2"), readme2_js, Ok(NumVal(11.0)));
      (Some("Lecture Scoping Test"), scopes_js, Ok(NumVal(1.0)))
    ]

let call_eval_tests =
  test_group "Call Evaluation"
    [
      (* TODO *)
      (Some("fib"), Printf.sprintf "(%s)(30)" fib_js, Ok(NumVal(832040.0)));
      (Some("Lexical Scope"), "const x = 5; const f = function(y){ return x + y; }; (function(z) {const x = 7; return f(6); })(0)", Ok(NumVal(11.0)));
      (Some("Lexical Scope Named"), "const x = 5; const f = function foo(y){ return x + y; }; (function goo(z) { const x = 7; return f(6); })(0)", Ok(NumVal(11.0)));
      (Some("fact"), Printf.sprintf "(%s)(5)" fact_js, Ok(NumVal(120.0)));
      
      (Some("basic"), "const f = function(x){ return x + 1; }; const r = f(2); r + 3", Ok(NumVal(6.0)));
      (Some("nest"), "const sum = function(x, y) { return x + y; }; const double = function(x) { return x * 2; }; double(sum(2, 3))", Ok(NumVal(10.0)));
      (Some("bool"), "const a = true; a", Ok(BoolVal(true)));
      (Some("bool fun"), "const isTrue = function(a) { return a === true; }; isTrue(true)", Ok(BoolVal(true)));
      (Some("edge"), "const f = function fib(x) { const fib = 3; return fib;}; f(1)", Ok(NumVal(3.0)));
    ]
