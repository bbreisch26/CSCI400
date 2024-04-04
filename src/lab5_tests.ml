open Javascript_parser;;
open Javascript_lexer;;
open Javascript_main;;
open Javascript_ast;;
open Util;;

let lexer_test_fun s =
  tokenize (Lexing.from_string s)


let lexer_tests =
  ("lexer_tests",
   lexer_test_fun,
   (=), (=),
   Some((fun (x : string) -> x), str_token_list),
   [
     (Some("simple js"),
      "1 + 2",
      Ok([NUMBER(1.0); ADD_OP; NUMBER(2.0); EOF]));
     (* TODO *)
     (Some("Simple binary"),
      "0b111 + 0B0001",
      Ok([NUMBER(7.0); ADD_OP; NUMBER(1.0); EOF]));
     (Some("Simple octal"),
      "0o7 / 0O12",
      Ok([NUMBER(7.0); DIV_OP; NUMBER(10.0); EOF]));
     (Some("Simple hex"),
      "0x1337 * 0XCafEBabE",
      Ok([NUMBER(4919.0); MUL_OP; NUMBER(3405691582.0); EOF]));
     (Some("Simple float"),
      "123.0 - .123",
      Ok([NUMBER(123.0); SUB_OP; NUMBER(0.123); EOF]));

   ])

let parser_tests =
  ("parser_tests",
   parse_string,
   eq_program, (=),
   Some((fun (x : string) -> x), str_program),
   [
      (Some("simple expression"),
        "1 + 2",
        Ok(ExprProgram(NoPos, BopExpr( NoPos,
                                       ValExpr(NoPos, NumVal(1.0)),
                                       PlusBop,
                                       ValExpr(NoPos, NumVal(2.0)) ))));

      (Some("Identity lambda"),
        "function (x) {return x;}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, VarExpr(NoPos, "x")), (* block_t *)
                                 None (* typ_t option *)
                                )))));
       (* TODO *)

  ])
