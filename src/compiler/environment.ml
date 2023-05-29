(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;

open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty);;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)

let allocate_closure (name : string) (env : environment) : environment = 
   let off, dict = env in
   let dict' = Map.String.add name (ArgLabelOffset(("closure_of_" ^ name), "1")) dict in
   (off-8, dict')
;;

let allocate_named_variable (name : string) (env : environment) : environment =
   let off,dict = env in 
   let dictionary' = Map.String.add name (ArgMemory(AddrByRegisterOffset(RBP,off))) dict in
   (off-8,dictionary')

 (* ignore name; ignore env; failwith "TODO: allocate_named_variable" (* TODO: delete this line *)*)
;;
let allocate_named_variable_in_function (name : string) (env : environment) (offset: int) :  environment * int=
   let off,dict = env in 
   let dictionary' = Map.String.add name (ArgMemory(AddrByRegisterOffset(RBP,offset))) dict in
   let new_offset = offset +8 in
   (off,dictionary') , new_offset
;;

let allocate_number_of_args (string_list : string list) (env : environment) : environment =
   let off, dict = env in 
   let size = List.length string_list in
   let address = Map.String.find_opt "parameters" dict in 
   match address with 
   | None -> let dictionary' = Map.String.add "parameters" (ArgConstant(string_of_int size)) dict in
      (off-8,dictionary')
   | Some(_) -> let dictionary' = Map.String.add "parameters" (ArgConstant(string_of_int size)) dict in
      (off, dictionary')
   
;;

let allocate_named_list_of_variables (string_list : string list) (env : environment): environment = 
   let original_offset = 16 in
   let rec loop (string_list : string list ) (env: environment) (offset: int) : environment = 
      match string_list with
      | [] -> env
      | element::list -> 
         let env', new_offset = allocate_named_variable_in_function element env offset in 
         loop list env' new_offset
      in
   loop string_list env original_offset
;;

(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
   let _,dict= env in
   let address = Map.String.find_opt name dict in 
   match address with 
   | None -> raise(UnboundVariable(name , env))
   | Some(x) -> x
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
  let off,dict = env in 
  (ArgMemory(AddrByRegisterOffset(RBP,off))),(off-8,dict)
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;

(* helpfer function thata takes list of strings an dadd an appropriate set of mappings*)