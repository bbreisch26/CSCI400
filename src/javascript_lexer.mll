{
   open Javascript_parser;;
   open Javascript_ast;;


   let strip_quotes (s : string) : (string*bool) =
     let len = String.length s in
     if ((len >= 2) && (String.get s 0)='"' && (String.get s (len-1))='"')
     then (String.sub s 1 (len-2),true) else (s,false)

   let my_unescaped s =
     let s = Str.global_replace (Str.regexp "[\\][\r][\n]") "" s in
     let s2 = Str.global_replace (Str.regexp "[\\][\n]") "" s in
     Scanf.unescaped s2
}

let digit = ['0' - '9']
let digit1 = ['1' - '9']
let exponent = ['e' 'E'] ['+' '-']? digit +

(* The type "token" is defined in Javascript_parser.mli *)
rule token = parse
  (* Comments *)
  | "/*" { entry_multilinecomment 0 "" lexbuf }
  | ("//" [^ '\n']*) as x {(let _ = count_newlines x lexbuf in (); token lexbuf)}

  (* Keywords *)
  | '}'         {RCB_KW}
  | '{'         {LCB_KW}
  | '('         {LP_KW}
  | ')'         {RP_KW}
  | ']'         {RSB_KW}
  | '['         {LSB_KW}
  | "function"  {FUNC_KW}
  | "undefined" {UNDEF_KW}
  | "return"    {RET_KW}
  | "let"       {LET_KW}
  | "const"     {CONST_KW}
  (* DONE: Add rules for true and false keywords *)
  | "true"      {TRUE_KW}
  | "false" 	{FALSE_KW}
  | "Infinity"  {INFINITY_KW}
  | "NaN"       {NAN_KW}
  | "log"       {LOG_KW}
  | "console"   {CONSOLE_KW}

  (* Operators *)
  | ';'         {SEMICOLON_OP}
  | '!'         {LOG_NOT_OP}
  | "||"        {LOG_OR_OP}
  | "&&"        {LOG_AND_OP}
  | "!=="       {NSTREQ_OP}
  | "==="       {STREQ_OP}
  | '>'         {GT_OP}
  | ">="        {GEQ_OP}
  | '<'         {LT_OP}
  | "<="        {LEQ_OP}
  (* DONE: Add rules for +, -, /, and * operators*)
  | '+'	        {ADD_OP}
  | '-'		{SUB_OP}
  | '/'		{DIV_OP}
  | '*'		{MUL_OP}
  | '='         {ASSIGN_OP}
  | '?'         {COND_OP}
  | ':'         {COLON_OP}
  | '.'         {DOT_OP}
  | ','         {COMMA_OP}

  (* Strings *)
  | '"'
     ([^ '\\' '"'] (* <- unescaped *)
     (* escape *)
     | '\\' (  ['\\' '\'' '"' 'n' 't' 'b' 'r' '\n']
            | ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
            )
     )*
    '"'
    as x {
           (let _ = count_newlines x lexbuf in STRING(my_unescaped (fst (strip_quotes x))))
         }

  (* Begin Numbers  *)
  (* DONE: Fix the rule for numbers *)
  (* Include support for: *)
  (* Leading zeros allowed according to NTD 4/2 help sesh*)
  (* - decimal: e.g., 1, 10 *)
  | ['0'-'9']+
  (* - binary: e.g., 0b1, 0B10 *)
  | ("0b" | "0B") ['0'-'1']+
  (* - octal: e.g., 0o1, 0O17 *)
  | ("0o" | "0O") ['0' - '7']+
  (* - hexadecimal: e.g., 0x1, 0Xff, 0x10aAfF *)
  | ("0x" | "0X") ['0'-'9' 'a'-'f' 'A'-'F']+
  (* - floating point: e.g., 123., 123.456, .123 *)
  | ['0'-'9']* '.' ['0'-'9']*
  (* - scientific notation: e.g., 123e5, 123.E5, 123.456e-5, .123e100 *)
  | ['0'-'9']* '.'? ['0'-'9']* exponent
   as x {NUMBER(js_float_of_string x )}

  (* End Numbers  *)


  (* Whitespace *)
  | ['\r' '\n' '\t' ' ']+
    as x {(let _ = count_newlines x lexbuf in (); token lexbuf)}

  (* Identifiers *)
  (* DONE: Fix the rule for identifiers *)
  (* see w3 schools article on javascript variables*)
  (* case sensitive, can contain letters, digits, underscores, and dollar signs*)
  (* must begin with letter, dollar sign, or underscore *)
  | (('$' | '_' | ['a'-'z' 'A'-'Z']) ['a'-'z' 'A'-'Z' '$' '_' '1'-'9']*)
     as x {IDENT(x)}

  (* End of File *)
  | eof { EOF }
  (* Lexing error *)
  | _ { lex_error "lexing error" lexbuf }

(* Multiline Comments *)
and entry_multilinecomment n x = parse
  | "/*" { entry_multilinecomment (n+1) (x^"/*") lexbuf }
  | "*/" { if (n=0) then ((); token lexbuf) else entry_multilinecomment (n-1) (x^"*/") lexbuf }
  | _ as c { if c='\n' then do_newline lexbuf;
                entry_multilinecomment n (Printf.sprintf "%s%c" x c) lexbuf }
