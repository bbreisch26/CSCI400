
(* Functional helpers *)
let compose f g =
  fun x -> f (g x)

(* String Conversions *)

let str_bool x =
  if x then "true" else "false"

let str_float x =
  Printf.sprintf "%f" x

let str_str (x:string) : string = x

let rec str_pair (f : 'a -> string) (g : 'b -> string) ((a,b) : ('a * 'b)) : string =
  "(" ^ (f a) ^ "," ^ (g b) ^ ")"


let str_tuple3 (f0 : 'x0 -> string) (f1 : 'x1 -> string) (f2 : 'x2 -> string)
      ((x0,x1,x2) : ('x0*'x1*'x2))
    : string =
  "("
  ^ (f0 x0) ^ ","
  ^ (f1 x1) ^ ","
  ^ (f2 x2)
  ^ ")"

let str_tuple4
      (f0 : 'x0 -> string) (f1 : 'x1 -> string)
      (f2 : 'x2 -> string) (f3 : 'x3 -> string)
      ((x0,x1,x2,x3) : ('x0*'x1*'x2*'x3))
    : string =
  "("
  ^ (f0 x0) ^ ","
  ^ (f1 x1) ^ ","
  ^ (f2 x2) ^ ","
  ^ (f3 x3)
  ^ ")"

let rec str_option (f : 'a -> string) (o : 'a option) : string =
   match o with
   | None -> ""
   | Some(a) -> (f a)

let str_int_option (o:int option) : string =
  match o with
    None -> ""
  | Some(a) -> (string_of_int a)

let rec str_x_list (f : 'a -> string) (il : 'a list) (comma : string) : string =
  fst (List.fold_left
         (fun (str,flag) i ->
           (str^(if flag then "" else comma)^(f i), false))
         ("",true)
         il)


let str_int_list (l:int list) : string =
  "["^(str_x_list string_of_int l "; ")^"]"

(* Comparison helpers *)
let eq_base (a : 'a) (b : 'a) : bool = (a = b)

let rec eq_option (f : 'a -> 'a -> bool) (a : 'a option) (b : 'a option) : bool =
   match (a,b) with
   | (None,None) -> true
   | (Some(a),Some(b)) -> (f a b)
   | _ -> false

let eq_pair (f1 : 'a -> 'a -> bool) (f2 : 'b -> 'b -> bool) ((p1a,p1b) : 'a * 'b) ((p2a,p2b) : 'a * 'b) : bool =
  ((f1 p1a p2a) && (f2 p1b p2b))

let eq_list (f : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list) : bool =
  try List.fold_left2 (fun res l1i l2i -> res && (f l1i l2i)) true l1 l2
  with _ -> false

(* Main helpers *)

let die_err (s : string) =
   output_string stderr s;
   output_string stderr "\n";
   exit 1

let open_input name =
  if "-" = name then stdin
  else open_in name
