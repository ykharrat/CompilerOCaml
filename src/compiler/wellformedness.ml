open Batteries;;
open HatchLanguage;;

open Printf;;

open Asts;;

exception IllFormed of string list;;


let rec var_exists (str : string) (lst : string list) : bool = 
(match lst with 
| [] -> false
| element :: lst' -> 
  if element = str then true
  else var_exists str lst')
;;

let rec map_errors (lst : expr list) (dict : string list) (errors : string list) (f) : string list =
(match lst with 
| [] -> errors
| expr :: lst' -> 
  let dict, errors' = f dict errors expr in
  map_errors lst' dict errors' f
  )
;;

let rec find_duplicates (dict : int Map.String.t) (args : string list) (function_name : string) (error : string list): int Map.String.t * string list =
  

  match (args : string list) with 
  | [] -> dict, error
  | element :: args' -> 
    let occurance = Map.String.find_opt element dict in
    (match occurance with 
    | None ->
      let dict' = Map.String.add element 1 dict in 
      find_duplicates dict' args' function_name error
    | Some _ ->
    let error' = error @ ["Function " ^ function_name ^ " declares a duplicate parameter " ^ element ^ "."] in
    find_duplicates dict args' function_name error'
    )
;;

let rec check_duplicate_params (lst : declaration list) : string list =  
  (match lst with 
  | [] -> []
  | element :: lst' -> 
    let DFunction (name, args, _) = element in
    let _, error_list = find_duplicates Map.String.empty args name [] in
    error_list @ check_duplicate_params lst'
  )
;;

let rec recurse_AST_unbound (dict : string list) (errors : string list) (expr : expr) : string list * string list =
  (match expr with
  | EInt _ -> dict, errors
  | EBool _ -> dict, errors
  | EUnaryOp (_, expr') -> recurse_AST_unbound dict errors expr' 
  | EBinaryOp (_, expr', expr'') -> 
    let _, errors' = recurse_AST_unbound dict errors expr' in
    let _, errors'' = recurse_AST_unbound dict errors expr'' in 
    dict , errors' @ errors''
  | ELet (name, expr', expr'') -> 
    let dict' = dict @ [name] in
    let _, errors' = recurse_AST_unbound dict' errors expr' in
    let _, errors'' = recurse_AST_unbound dict' errors expr'' in 
    dict' , errors' @ errors''
  | EVar var -> 
    if var_exists var dict then dict, errors
    else 
      let errors' = ["Unbound variable " ^ var ^ "."] in
      dict, errors' @ errors
  | EIf (expr', expr'', expr''') -> 
    let _, errors' = recurse_AST_unbound dict errors expr' in
    let _, errors'' = recurse_AST_unbound dict errors expr'' in 
    let _, errors''' = recurse_AST_unbound dict errors expr''' in 
    dict, errors' @ errors'' @ errors'''
  (* | ECall (_, lst) -> 
    let concat_errors = map_errors lst dict errors recurse_AST_unbound in
    dict, concat_errors *)
  | ETuple lst -> 
    let concat_errors = map_errors lst dict errors recurse_AST_unbound in
    dict, concat_errors
  | EAppl (expr1, expr2, _) -> 
    let _, errors' = recurse_AST_unbound dict errors expr1 in
    let _, errors'' = recurse_AST_unbound dict errors expr2 in 
    dict , errors' @ errors''
  | ESet (_, _, _) -> dict, errors
  )
;;

let rec function_name_TBD (env : string list) (fn : declaration) : string list=
let DFunction(_, args, expr) = fn in
let dict' = args @ env in

let _, errors = recurse_AST_unbound dict' [] expr in
errors
;; 

let generate_env_fns (decs : declaration list) : string list =
  let env = [] in
  let rec loop decs env =
    match decs with 
    | [] -> env
    | element :: decs' -> 
      let DFunction(name, _, _) = element in
      let env' = env @ [name] in
      loop decs' env'
  in
  loop decs env
;;

let rec recurse_AST (fns : declaration list) (dict : int Map.String.t) (errors : string list) : int Map.String.t * string list =
(match fns with 
| [] -> dict, errors
| fn :: fns' -> 
  let DFunction(name, args, _) = fn in
  let exists = Map.String.find_opt name dict in
  (match exists with 
  | None -> let dict' = Map.String.add name (List.length args) dict in
    recurse_AST fns' dict' errors
  | Some _ -> 
    let errors' = ["Duplicate definition of function "^ name ^ "."] @ errors in
    recurse_AST fns' dict errors'
  ))
;;

(* let rec error_msg_numargs_notdefined (dict : int Map.String.t) (expr : expr) : string list =
(match expr with
| EInt _ -> []
| EBool _ -> []
| EUnaryOp (_, expr') -> error_msg_numargs_notdefined dict expr'
| EBinaryOp (_, expr', expr'') -> 
  (error_msg_numargs_notdefined dict expr') @ (error_msg_numargs_notdefined dict expr'')
| ELet (_, expr', expr'') -> 
  (error_msg_numargs_notdefined dict expr') @ (error_msg_numargs_notdefined dict expr'')
| EVar _ -> []
| EIf (expr', expr'', expr''') -> 
  (error_msg_numargs_notdefined dict expr') @ (error_msg_numargs_notdefined dict expr'') @ (error_msg_numargs_notdefined dict expr''')
| ECall (str, lst) -> 
  let num_args = Map.String.find_opt str dict in
  let concat_errors = List.concat (List.map (error_msg_numargs_notdefined dict) lst) in
  (match num_args with 
  | None -> ["Function " ^ str ^ " is not defined."] @ concat_errors
  | Some n -> 
    if n = (List.length lst) then concat_errors
    else ["Function " ^ str ^ " is called with an incorrect number of arguments."] @ concat_errors
  )
| ETuple lst -> 
  List.concat (List.map (error_msg_numargs_notdefined dict) lst)
  )
;; *)

(* This function produces a list of compile-time errors found in a program. *)
let check_program_for_errors (p : program) : string list =
  let Program(fns, main) = p in
  let env = generate_env_fns fns in
  let _, error_num_args = recurse_AST fns Map.String.empty [] in
  (* let expr_list = List.map (fun (DFunction (_, _, expr)) -> expr) fns in *)
  (* let error_msg_list_body = List.concat (List.map (error_msg_numargs_notdefined dict) expr_list) in
  let error_msg_list_main = error_msg_numargs_notdefined dict main in *)
  let error_unbound = List.concat (List.map (function_name_TBD env) fns) in
  
  let _, error_unbound_main = recurse_AST_unbound env [] main in
  let duplicate_params = check_duplicate_params fns in
  (* error_msg_list_main @ *) error_unbound_main @ (* error_msg_list_body @ *) error_num_args @ error_unbound @ duplicate_params

;;

(* This function will check a program for compile-time errors.  If any errors
  are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
let errors = check_program_for_errors p in
if List.is_empty errors then () else raise (IllFormed errors);
;;
