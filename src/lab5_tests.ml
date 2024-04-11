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
     (* Done *)
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
      "123.0 - .123 + 123.456",
      Ok([NUMBER(123.0); SUB_OP; NUMBER(0.123); ADD_OP; NUMBER(123.456); EOF]));
     (Some("Simple sci notation"),
      "123e0 + 123.E5 + 123.456e-5 + .123e1",
      Ok([NUMBER(123.0); ADD_OP; NUMBER(12300000.0); ADD_OP; NUMBER(0.00123456); ADD_OP; NUMBER(1.23); EOF]));

    (Some("floating point equation"),
      "(123.0 - .123) * (5.0 + 1.2)",
      Ok([LP_KW; NUMBER(123.000000); SUB_OP; NUMBER(0.123000); RP_KW; MUL_OP; LP_KW; NUMBER(5.000000); ADD_OP; NUMBER(1.200000); RP_KW; EOF]));
    (Some("boolean equation"),
      "!(true || !false) && (5 >= 4)",
      Ok([LOG_NOT_OP; LP_KW; TRUE_KW; LOG_OR_OP; LOG_NOT_OP; FALSE_KW; RP_KW; LOG_AND_OP; LP_KW; NUMBER(5.0); GEQ_OP; NUMBER(4.0); RP_KW; EOF]));
    (Some("list expression"),
      "[1; 2; 0b111; 0x1a9D; 0.5]",
      Ok([LSB_KW; NUMBER(1.0); SEMICOLON_OP; NUMBER(2.0); SEMICOLON_OP; NUMBER(7.0); SEMICOLON_OP; NUMBER(6813.0); SEMICOLON_OP; NUMBER(0.5); RSB_KW; EOF]));
    (Some("comment"),
      "// this is a comment",
      Ok([EOF]));
    (Some("multi-line comment"),
      "/*
      this is a multi-line comment
      
      lalalalalalala
      */",
      Ok([EOF]));
    (Some("string assignment"),
      "let my_string = \"this is a string\";",
      Ok([LET_KW; IDENT("my_string"); ASSIGN_OP; STRING("this is a string"); SEMICOLON_OP; EOF]));
    (Some("simple function"),
      "function my_function(num1, num2) {
        // this function will add 2 input numbers and print the sum
        const sum = num1 + num2;
        console.log(sum);
      }",
      Ok([FUNC_KW; IDENT("my_function"); LP_KW; IDENT("num1"); COMMA_OP; IDENT("num2"); RP_KW; LCB_KW; CONST_KW; IDENT("sum"); ASSIGN_OP;
      IDENT("num1"); ADD_OP; IDENT("num2"); SEMICOLON_OP; CONSOLE_KW; DOT_OP; LOG_KW; LP_KW; IDENT("sum"); RP_KW; SEMICOLON_OP; RCB_KW; EOF]));

    (Some("function call"),
      "my_function(5, 7);",
      Ok([IDENT("my_function"); LP_KW; NUMBER(5.0); COMMA_OP; NUMBER(7.0); RP_KW; SEMICOLON_OP; EOF]));
    (Some("non existent operator"),
      "
      let x = 5 % 2;
      ",
      Error(Javascript_ast.Lexing_error("Error in '' on line 2 col 12: lexing error\n")));
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
       (Some("Variable declaration with let"),
       "let a = 5;",
       Ok(StmtProgram(NoPos, 
                      LetStmt(NoPos, "a", 
                              ValExpr(NoPos, NumVal(5.0))), 
                      ExprProgram(NoPos,
                                  ValExpr(NoPos, UndefVal)))));

      (Some("Ternary operation"),
      "a > b ? a : b",
      Ok(ExprProgram(NoPos, 
                      IfExpr(NoPos, 
                            BopExpr(NoPos, VarExpr(NoPos, "a"), GtBop, VarExpr(NoPos, "b")),
                            VarExpr(NoPos, "a"),
                            VarExpr(NoPos, "b")))));

      (Some("Complex expression"),
      "3 + 4 * (2 - 1) / 5",
      Ok(ExprProgram(NoPos,
                      BopExpr(NoPos,
                              ValExpr(NoPos, NumVal(3.0)),
                              PlusBop,
                              BopExpr(NoPos,
                                      BopExpr(NoPos, 
                                              ValExpr(NoPos, NumVal(4.0)), 
                                              TimesBop, 
                                              BopExpr(NoPos, 
                                                      ValExpr(NoPos, NumVal(2.0)), 
                                                      MinusBop, 
                                                      ValExpr(NoPos, NumVal(1.0)))),
                                      DivBop,
                                      ValExpr(NoPos, NumVal(5.0)))))));
        (Some("Boolean expression"),
        "a > b",
        Ok(ExprProgram(NoPos,
                      BopExpr(NoPos, 
                              VarExpr(NoPos, "a"), 
                              GtBop,
                              VarExpr(NoPos, "b")))));                         
        (Some("Complex Boolean expression"),
        "a < b || c >= d",
        Ok(ExprProgram(NoPos,
                      BopExpr(NoPos,
                             BopExpr(NoPos,
                                    VarExpr(NoPos, "a"),
                                    LtBop,
                                    VarExpr(NoPos, "b")),
                             OrBop,
                             BopExpr(NoPos,
                                    VarExpr(NoPos, "c"),
                                    GteBop,
                                    VarExpr(NoPos, "d"))))));
        (Some("Return expression"),
        "{ return hello ; }",
        Ok(ExprProgram(NoPos,
                      BlockExpr(NoPos,
                               ReturnBlock(NoPos, 
                                           VarExpr(NoPos, "hello"))))));
        (Some("Precedence Test"),
         "!((3 === 3)===true)",
         Ok(ExprProgram(NoPos,
                        UopExpr(NoPos,
                                 NotUop,
                                 BopExpr(NoPos,
                                           BopExpr(NoPos,
                                                   ValExpr(NoPos, NumVal(3.0)),
                                                   EqBop,
                                                   ValExpr(NoPos, NumVal(3.0))),
                                           EqBop,
                                           ValExpr(NoPos, BoolVal(true)))))));
        (Some("Console Log expression"),
        "console.log(HelloWorld)",
        Ok(ExprProgram(NoPos,
                      PrintExpr(NoPos, 
                               VarExpr(NoPos, 
                                      "HelloWorld")))));
        (Some("Const assignment expression"),
        "const x = 10;",
        Ok(StmtProgram(NoPos,
                      ConstStmt(NoPos, 
                               "x",
                               ValExpr(NoPos, 
                                      NumVal(10.0))),
                      ExprProgram(NoPos, ValExpr(NoPos, UndefVal)))));
        (Some("Assignment Expression"),
        "x = y;",
        Ok(StmtProgram(NoPos,
                      AssignStmt(NoPos,
                                VarExpr(NoPos, "x"),
                                VarExpr(NoPos, "y")),
                      ExprProgram(NoPos, ValExpr(NoPos, UndefVal)))));
        (Some("Function expression"),
        "function f(x) { return hello; }",
        Ok(ExprProgram(NoPos,
                      FuncExpr(NoPos, 
                              (Some("f"), [("x", None)], ReturnBlock(NoPos, 
                                                                  VarExpr(NoPos, "hello")), None)))));
        (Some("Field List expression"),
        "{a:int, b:char, c:string}",
        Ok(ExprProgram(NoPos,
                      ObjectExpr(NoPos,
                                ("a", VarExpr(NoPos, "int"))::("b", VarExpr(NoPos, "char"))::("c", VarExpr(NoPos, "string"))::[]))));
  ])
