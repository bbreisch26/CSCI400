open Javascript_ast
open Util

(* obtain most recent binding of variable x in environment env *)
let read_environment (env:environment_t) (x:string)
    : (var_access_t*value_t) option =
  try Some(StringMap.find x env)
  with _ -> None

(* create a new binding for variable x into the environment env *)
let bind_environment (env:environment_t) (x:string) (access:var_access_t) (value:value_t)
    : environment_t =
  StringMap.add x (access,value) env


(* empty environment - used for testing *)
let empty_env = StringMap.empty



and str_environment_simple (e:environment_t) : string =
  str_environment e
