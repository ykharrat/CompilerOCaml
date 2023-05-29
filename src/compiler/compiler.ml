(** This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;
open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;
  let checks : bool = true;;
    let string_of_twice_int (n : int) : string =
      Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2))
    ;;

      let error_int () : instruction list = 
        if not checks then [] else
          let save_RAX = [AsmMov(ArgRegister(R11),ArgRegister(RAX))] in
          let is_int_RAX =  
            [AsmAnd(ArgRegister(RAX),ArgConstant("1"))] @ 
            [AsmSal(ArgRegister(RAX), ArgConstant("63"))] @ 
            [AsmMov(ArgRegister(R10),ArgConstant("0x7FFFFFFFFFFFFFFF"))] @
            [AsmOr(ArgRegister(RAX), ArgRegister(R10))] @
            [AsmMov(ArgRegister(R10),ArgConstant("0x8000000000000000"))] @
            [AsmAdd(ArgRegister(RAX), ArgRegister(R10))] in 
          let error_message = [AsmMov(ArgRegister(RDI),ArgConstant("1"));
                               AsmCall("stopWithError")];

          in    
          let name_equal_than = fresh_name "name_equal" in


          save_RAX @ is_int_RAX @  [AsmCmp(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
                                    AsmJe(name_equal_than)] @ error_message @ 
          [AsmLabel(name_equal_than);
           AsmMov(ArgRegister(RAX),ArgRegister(R11))] 
        ;;

        let generate_closure (decl : declaration) : instruction list =
          (match decl with 
          | DFunction (name, lst, _) -> 
            let size = List.length lst in
            [AsmAlign("8")] @
            [AsmLabel("closure_of_" ^ name)] @
            [AsmDq("0x8000000000000000, 0x0000000000000000, " ^ string_of_int size ^ ", " ^ name)] (* the second word is a GC 64 bit word*)
          )
        ;;


        let check_rightmost_1 () : instruction list = 
          [AsmAnd(ArgRegister(RAX),ArgConstant("1"))] @ 
          [AsmSal(ArgRegister(RAX), ArgConstant("63"))] @ 
          [AsmMov(ArgRegister(R10),ArgConstant("0x7FFFFFFFFFFFFFFF"))] @
          [AsmOr(ArgRegister(RAX), ArgRegister(R10))]
        ;;
          
        let check_rightmost_0 () : instruction list = 
          [AsmAnd(ArgRegister(RAX),ArgConstant("1"))] @ 
          [AsmSal(ArgRegister(RAX), ArgConstant("63"))] @ 
          [AsmMov(ArgRegister(R10),ArgConstant("0x7FFFFFFFFFFFFFFF"))] @
          [AsmOr(ArgRegister(RAX), ArgRegister(R10))] @
          [AsmMov(ArgRegister(R10),ArgConstant("0x8000000000000000"))] @
          [AsmAdd(ArgRegister(RAX), ArgRegister(R10))]
        ;;
        let error_bool () : instruction list = 
          if not checks then [] else
            let save_RAX = [AsmMov(ArgRegister(R11),ArgRegister(RAX))] in
            let is_Bool_RAX =  
              [AsmMov(ArgRegister(R9), ArgRegister(RAX))] @
              [AsmSar(ArgRegister(R9), ArgConstant("1"))] @ 
              [AsmAnd(ArgRegister(RAX), ArgConstant("1"))] @ 
              [AsmAnd(ArgRegister(R9), ArgConstant("1"))] @ 
              [AsmAnd(ArgRegister(RAX), ArgRegister(R9))] @ 
              [AsmSal(ArgRegister(RAX), ArgConstant("63"))] @ 
              [AsmMov(ArgRegister(R10),ArgConstant("0x7FFFFFFFFFFFFFFF"))] @
              [AsmOr(ArgRegister(RAX), ArgRegister(R10))]
            in
            let error_message = [AsmMov(ArgRegister(RDI),ArgConstant("2"));
                                  AsmCall("stopWithError")];

            in    
            let name_equal_than = fresh_name "name_equal" in
            save_RAX @ is_Bool_RAX @  [AsmCmp(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"));
                                        AsmJe(name_equal_than)] @ error_message @ 
            [AsmLabel(name_equal_than);
              AsmMov(ArgRegister(RAX),ArgRegister(R11))]
        ;;
(* for gull make sure to save on the stack not machine pointers but bird pointers for checking *)              
              let error_tuple () : instruction list =
                if not checks then [] else
                  let save_RAX = [AsmMov(ArgRegister(R11),ArgRegister(RAX))] in
                  let not_tuple = [AsmSub(ArgRegister(RAX), ArgConstant("1"));
                   AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)));
                   AsmMov(ArgRegister(R9), ArgConstant("0x7FFFFFFFFFFFFFFF"));
                   AsmOr(ArgRegister(RAX), ArgRegister(R9))] in
                  let name_equal_than2 = fresh_name "name_equal" in
                  let is_pointer =  
                    let rightmost_1 = check_rightmost_1 () in
                    let rightmost_0 = check_rightmost_0 () in
                    let save_number1 = [AsmMov(ArgRegister(R11), ArgRegister(RAX))] in
                    let save_number2 = [AsmMov(ArgRegister(R9), ArgRegister(RAX))] in
                    save_number1 @ rightmost_1 @ save_number2 @
                    [AsmMov(ArgRegister(RAX), ArgRegister(R11))] @
                    [AsmSar(ArgRegister(RAX), ArgConstant("1"))] @
                    rightmost_0 @
                    [AsmMov(ArgRegister(R10), ArgRegister(R9))] @
                    [AsmAnd(ArgRegister(RAX), ArgRegister(R10))] in
                  let error_message = [AsmMov(ArgRegister(RDI),ArgConstant("3"));
                                       AsmCall("stopWithError")];
                  in    
                  let name_equal_than1 = fresh_name "name_equal" in
                  save_RAX @ is_pointer @  [AsmMov(ArgRegister(R8), ArgConstant("0xFFFFFFFFFFFFFFFF")); AsmCmp(ArgRegister(RAX), ArgRegister(R8));
                                             AsmJe(name_equal_than1)] @ error_message @
                  [AsmLabel(name_equal_than1);
                   AsmMov(ArgRegister(RAX),ArgRegister(R11))] @
                   not_tuple @
                   [AsmMov(ArgRegister(R8), ArgConstant("0x7FFFFFFFFFFFFFFF")); AsmCmp(ArgRegister(RAX), ArgRegister(R8)); AsmJe(name_equal_than2)] @
                   error_message @
                  [AsmLabel(name_equal_than2); AsmMov(ArgRegister(RAX),ArgRegister(R11))]
              ;;


              let rec mark_tail (e : expr) : expr =
                match e with
                | EInt a -> EInt a
                | EBool b -> EBool b
                | EUnaryOp (op, expr1) -> EUnaryOp (op, mark_tail expr1)
                | EBinaryOp (op, a , b) -> 
                    EBinaryOp(op, mark_tail a, mark_tail b)
                | ETuple e -> 
                  let e' = List.map mark_tail e in
                  ETuple(e')
                | ELet (l, a, b) -> ELet (l, mark_tail a, mark_tail b)
                | EVar v -> EVar v
                | EIf (expr1, expr2, expr3) -> 
                  (match expr2, expr3 with 
                  | EAppl(closure1, arg1, _), EAppl(closure2, arg2, _) -> EIf(expr1, EAppl(closure1, arg1, true), EAppl(closure2, arg2, true))
                  | EAppl(closure, arg, _), _ -> EIf (expr1, EAppl(closure, arg, true), mark_tail expr3)
                  | _, EAppl(closure, arg, _) -> EIf (expr1, mark_tail expr2, EAppl(closure, arg, true))
                  | _, _ -> EIf(mark_tail expr1, mark_tail expr2, mark_tail expr3)
                  )
                | ESet (expr1, expr2, expr3) -> ESet(mark_tail expr1, mark_tail expr2, mark_tail expr3)
                | EAppl (closure, arg, _) -> 
                  EAppl(closure, arg, true)
                  (* match arg with
                  | EAppl (_, _, _) ->
                    EAppl (closure, mark_tail arg, true)
                  | _ -> EAppl (closure, mark_tail arg, false) *)
              ;;

            let mark_tail_starter (p : program) : program =
              let Program(decs, main) = p in
              let main' = mark_tail main in
              Program(decs, main')
            ;;


              let error_closure () : instruction list = 
                if not checks then [] else
                  let save_RAX = [AsmMov(ArgRegister(R11),ArgRegister(RAX))] in
                  let is_closure = [AsmSub(ArgRegister(RAX), ArgConstant("1"));
                   AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)));
                   AsmMov(ArgRegister(R9), ArgConstant("0x7FFFFFFFFFFFFFFF"));
                   AsmOr(ArgRegister(RAX), ArgRegister(R9))] in
                   let error_message = [AsmMov(ArgRegister(RDI),ArgConstant("5")); AsmCall("stopWithError")] in
                   let name_equal_than1 = fresh_name "name_equal" in
                   let name_equal_than2 = fresh_name "name_equal" in
                   let is_pointer =  
                    let rightmost_1 = check_rightmost_1 () in
                    let rightmost_0 = check_rightmost_0 () in
                    let save_number1 = [AsmMov(ArgRegister(R11), ArgRegister(RAX))] in
                    let save_number2 = [AsmMov(ArgRegister(R9), ArgRegister(RAX))] in
                    save_number1 @ rightmost_1 @ save_number2 @
                    [AsmMov(ArgRegister(RAX), ArgRegister(R11))] @
                    [AsmSar(ArgRegister(RAX), ArgConstant("1"))] @
                    rightmost_0 @
                    [AsmMov(ArgRegister(R10), ArgRegister(R9))] @
                    [AsmAnd(ArgRegister(RAX), ArgRegister(R10))] in 
                   save_RAX @ 
                   is_pointer @
                   [AsmMov(ArgRegister(R8), ArgConstant("0xFFFFFFFFFFFFFFFF")); AsmCmp(ArgRegister(RAX), ArgRegister(R8)); AsmJe(name_equal_than1)] @ 
                   error_message @ 
                   [AsmLabel(name_equal_than1); AsmMov(ArgRegister(RAX),ArgRegister(R11))] @
                   is_closure @ 
                   [AsmMov(ArgRegister(R8), ArgConstant("0xFFFFFFFFFFFFFFFF")); AsmCmp(ArgRegister(RAX), ArgRegister(R8)); AsmJe(name_equal_than2)] @ 
                   error_message @ 
                  [AsmLabel(name_equal_than2); AsmMov(ArgRegister(RAX),ArgRegister(R11))]
              ;;
                let error_checking1 (e : expr) : instruction list = 
                  (match e with
                   | EInt _ -> []
                   | EBool _ -> []
                   | EUnaryOp (op, _) -> 
                     (match op with
                      | OpAfter 
                      | OpBefore -> 
                        error_int ()
                      | OpIsInt 
                      | OpIsBool 
                      | OpPrint -> []
                      | OpIsTuple -> []
                      | OpTupleSize -> []
                     )
                   | EBinaryOp (op, _, _) -> 
                     (match op with
                      | OpPlus 
                      | OpMinus  
                      | OpTimes  
                      | OpLessThan 
                      | OpGreaterThan -> 
                        error_int ()
                      | OpEqualTo -> []
                      | OpAnd 
                      | OpOr ->
                        error_bool ()
                      | OpTupleIndex -> error_tuple()
                     )
                   | ELet (_, _, _) -> []
                   | EVar _ -> []
                   | EIf (_, _, _) -> 
                     error_bool ()
                   (* | ECall (_, _) -> [] *)
                   | ETuple (_) -> []
                   | EAppl (_, _, _) -> error_closure()
                   | ESet (_, _, _) -> error_tuple()
                  )
                   ;;

                  let error_checking2 (e : expr) : instruction list = 
                    (match e with
                     | EInt _ -> []
                     | EBool _ -> []
                     | EUnaryOp (op, _) -> 
                       (match op with
                        | OpAfter 
                        | OpBefore -> []
                        | OpIsInt 
                        | OpIsBool 
                        | OpPrint -> []
                        | OpIsTuple -> []
                        | OpTupleSize -> []
                       )
                     | EBinaryOp (op, _ , _) -> 
                       (match op with
                        | OpPlus 
                        | OpMinus  
                        | OpTimes  
                        | OpLessThan 
                        | OpGreaterThan -> []
                        | OpEqualTo -> []
                        | OpAnd 
                        | OpOr -> []
                        | OpTupleIndex -> 
                          error_int()
                       )
                     | ELet (_, _, _) -> []
                     | EVar _ -> []
                     | EIf (_, _, _) -> 
                       error_bool ()
                     (* | ECall (_, _) -> [] *)
                     | ETuple (_) -> []
                     | EAppl (_, _, _) -> []
                     | ESet (_, _, _) -> error_int()
                    )
                     ;;
                    let call_gc : instruction list = 
                      [AsmMov(ArgMemory(AddrByLabel("end_of_stack")), ArgRegister(RSP)); (*Assuming that R10 holds the size that we need that we need *)
                      AsmMov(ArgRegister(RDI),ArgRegister(R10));
                      AsmPush(ArgRegister(R10));
                      AsmPush (ArgRegister (RAX));
                      AsmCall("gc");
                      AsmPop(ArgRegister(RAX));
                      AsmPop(ArgRegister(R10));
                      ]
                    ;;

                      let rec compile_expression (env : environment) (e : expr) : instruction list =
                        match e with 
                        | EInt(x) ->  [AsmMov ((ArgRegister RAX), ArgConstant(string_of_twice_int (x)))]
                        | EUnaryOp(op, e') -> (match op with
                            | OpAfter -> compile_expression env e' @ error_checking1(e) @ [AsmAdd(ArgRegister RAX, ArgConstant (string_of_int 2))]
                            | OpBefore-> compile_expression env e' @ error_checking1(e) @ [AsmSub(ArgRegister RAX, ArgConstant (string_of_int 2))]
                            | OpIsInt ->  compile_expression env e' @ error_checking1(e) @ check_rightmost_0 ()
                            | OpIsBool -> 
                              let rightmost = check_rightmost_1 () in
                              let arg1, env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg1, ArgRegister(RAX))] in
                              let arg2, _ = allocate_temp_variable env' in
                              let save_number2 = [AsmMov(arg2, ArgRegister(RAX))] in
                              compile_expression env e' @  error_checking1(e) @ save_number1 @ rightmost @ save_number2 @
                              [AsmMov(ArgRegister(RAX), arg1)] @
                              [AsmSar(ArgRegister(RAX), ArgConstant("1"))] @
                              rightmost @
                              [AsmMov(ArgRegister(R10), arg2)] @
                              [AsmAnd(ArgRegister(RAX), ArgRegister(R10))] @
                              [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                              [AsmMov(arg1, ArgRegister(R10))] @ (* zero this 9a7ba out *)
                              [AsmMov(arg2, ArgRegister(R10))] (* zero this 9a7ba out *)
                            | OpPrint ->
                              let arg = compile_expression env e'  in
                              let store_arg = [AsmMov (ArgRegister (RDI), ArgRegister (RAX))] in
                              arg @
                              store_arg @
                              [AsmPush (ArgRegister (RAX))] @
                              [AsmCall ("printValue")] @
                              [AsmPop (ArgRegister(RAX))]
                            | OpIsTuple ->
                              let rightmost_1 = check_rightmost_1 () in
                              let rightmost_0 = check_rightmost_0 () in
                              let arg1, env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg1, ArgRegister(RAX))] in
                              let arg2, _ = allocate_temp_variable env' in
                              let save_number2 = [AsmMov(arg2, ArgRegister(RAX))] in
                              let is_tuple = [AsmSub(ArgRegister(RAX), ArgConstant("1"));
                                              AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)));
                                              AsmMov(ArgRegister(R9), ArgConstant("0x7FFFFFFFFFFFFFFF"));
                                              AsmOr(ArgRegister(RAX), ArgRegister(R9));
                                              AsmMov(ArgRegister(R8), ArgConstant("0x8000000000000000"));
                                              AsmXor(ArgRegister(RAX), ArgRegister(R8))] in
                              let name_equal_than1 = fresh_name "name_equal" in
                              compile_expression env e' @  error_checking1(e) @ save_number1 @ rightmost_1 @ save_number2 @
                              [AsmMov(ArgRegister(RAX), arg1)] @
                              [AsmSar(ArgRegister(RAX), ArgConstant("1"))] @
                              rightmost_0 @
                              [AsmMov(ArgRegister(R10), arg2)] @
                              [AsmAnd(ArgRegister(RAX), ArgRegister(R10))] @
                              (* check last bit *)
                              [AsmMov(ArgRegister(R8), ArgConstant("0x7FFFFFFFFFFFFFFF")); AsmCmp(ArgRegister(RAX), ArgRegister(R8));
                                             AsmJe(name_equal_than1)] @
                              [AsmMov(ArgRegister(RAX),arg1)] @
                              is_tuple @
                              [AsmLabel(name_equal_than1)] @
                              [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                              [AsmMov(arg1, ArgRegister(R10))] (* zero this 9a7ba out *)

                            | OpTupleSize ->
                              compile_expression env e' @
                              [AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)))]
                          )
                        | EVar(str) -> 
                          let arg = find_named_variable str env in 
                          [AsmMov(ArgRegister RAX, arg)]
                        | EBool (racks) -> (match racks with
                            | true -> [AsmMov(ArgRegister(RAX), ArgConstant("0xFFFFFFFFFFFFFFFF"))]
                            | false -> [AsmMov(ArgRegister(RAX), ArgConstant("0x7FFFFFFFFFFFFFFF"))]
                          )
                        | ELet (str, expr1, expr2)->  
                          let env'= allocate_named_variable str env in
                          let instrs1 = compile_expression env expr1 in
                          let instsglue = [AsmMov(find_named_variable str env', ArgRegister(RAX))] in
                          let instrs2 = compile_expression env' expr2 in 
                          instrs1 @ instsglue @ instrs2 @
                          [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                          [AsmMov(find_named_variable str env', ArgRegister(R10))] (* zero this 9a7ba out *)
                        | EBinaryOp(op, expr1, expr2) -> (match op with 
                            | OpPlus -> 
                              let number1_instr = compile_expression env expr1 @ error_checking1(e) in
                              let arg,env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg , ArgRegister(RAX))] in
                              let number2_instr = compile_expression env' expr2 @ error_checking1(e)in 
                              let glue_instr = [AsmAdd(ArgRegister(RAX),arg)] in
                              number1_instr @ save_number1 @ number2_instr @ glue_instr
                            | OpMinus ->      
                              let arg,env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg , ArgRegister(RAX))] in
                              let number2_instr = compile_expression env' expr2 @ error_checking1(e) in
                              let number1_instr = compile_expression env expr1  @ error_checking1(e) in 
                              let glue_instr = [AsmSub(arg,ArgRegister(RAX));
                                                AsmMov(ArgRegister(RAX),arg)] in
                              number1_instr @  save_number1 @ number2_instr @ glue_instr
                            | OpTimes -> 
                              let number1_instr = compile_expression env expr1 @ error_checking1(e) @ [AsmSar(ArgRegister(RAX),ArgConstant("1"))] in
                              let arg,env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg , ArgRegister(RAX))] in
                              let number2_instr = compile_expression env' expr2 @ error_checking1(e) in 
                              let glue_instr = [AsmIMul(ArgRegister(RAX),arg)] in
                              number1_instr @ save_number1 @ number2_instr @ glue_instr
                            | OpLessThan -> 
                              let arg,env' = allocate_temp_variable env in
                              let save_number2 = [AsmMov(arg , ArgRegister(RAX))] in
                              let name_less_than = fresh_name "name_less" in
                              let name_end_less = fresh_name "name_end_less" in 
                              let expr2_instr = compile_expression env' expr2 @ error_checking1(e) in 
                              let expr1_instr = compile_expression env expr1 @ error_checking1(e) in 
                              expr1_instr @ save_number2 @  expr2_instr @ 
                              [AsmCmp(ArgRegister(RAX), arg);
                               AsmJle(name_less_than);
                               AsmMov(ArgRegister(RAX),ArgConstant("0xFFFFFFFFFFFFFFFF"));
                               AsmJmp(name_end_less)]@
                              [AsmLabel(name_less_than);
                               AsmMov(ArgRegister(RAX),ArgConstant("0x7FFFFFFFFFFFFFFF"));
                               AsmLabel(name_end_less)] 

                            | OpGreaterThan -> 
                              let arg,env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg , ArgRegister(RAX))] in
                              let name_greater_than = fresh_name "name_greater" in
                              let name_end_greater = fresh_name "name_end_greater" in 
                              let expr2_instr = compile_expression env' expr2 @ error_checking1(e) in 
                              let expr1_instr = compile_expression env expr1 @ error_checking1(e) in 
                              expr1_instr @ save_number1 @  expr2_instr @ 
                              [AsmCmp(ArgRegister(RAX), arg);
                               AsmJge(name_greater_than);
                               AsmMov(ArgRegister(RAX),ArgConstant("0xFFFFFFFFFFFFFFFF"));
                               AsmJmp(name_end_greater)]@
                              [AsmLabel(name_greater_than);
                               AsmMov(ArgRegister(RAX),ArgConstant("0x7FFFFFFFFFFFFFFF"));
                               AsmLabel(name_end_greater)] 
                            | OpEqualTo -> 
                              let arg,env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg , ArgRegister(RAX))] in
                              let name_equal_than = fresh_name "name_equal" in
                              let name_end_equal = fresh_name "name_end_equal" in 
                              let expr2_instr = compile_expression env' expr2 in 
                              let expr1_instr = compile_expression env expr1 in 
                              expr1_instr @ save_number1 @  expr2_instr @ 
                              [AsmCmp(ArgRegister(RAX), arg);
                               AsmJe(name_equal_than);
                               AsmMov(ArgRegister(RAX),ArgConstant("0x7FFFFFFFFFFFFFFF"));
                               AsmJmp(name_end_equal)]@
                              [AsmLabel(name_equal_than);
                               AsmMov(ArgRegister(RAX),ArgConstant("0xFFFFFFFFFFFFFFFF"));
                               AsmLabel(name_end_equal)] 
                            | OpAnd -> 
                              let arg,env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg , ArgRegister(RAX))] in 
                              let expr2_instr = compile_expression env' expr2 @ error_checking1(e) in 
                              let expr1_instr = compile_expression env expr1 @ error_checking1(e) in
                              expr1_instr  @ save_number1 @ expr2_instr @ [AsmAnd(arg,ArgRegister(RAX));
                                                                           AsmMov(ArgRegister(RAX),arg)]
                            | OpOr->       
                              let arg,env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg , ArgRegister(RAX))] in 
                              let expr2_instr = compile_expression env' expr2 @ error_checking1(e) in 
                              let expr1_instr = compile_expression env expr1 @ error_checking1(e) in
                              expr1_instr  @ save_number1 @ expr2_instr @ [AsmOr(arg,ArgRegister(RAX));
                                                                           AsmMov(ArgRegister(RAX),arg)]
                            | OpTupleIndex ->
                              let arg1, env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg1, ArgRegister(RAX))] in
                              let ptr_offset = 
                                [AsmSal(ArgRegister(RAX), ArgConstant("3"));
                                 AsmMov(ArgRegister(R11), arg1)] @
                                 [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                                 [AsmMov(arg1, ArgRegister(R10))] @ (* zero this 9a7ba out *)
                                 [AsmSub(ArgRegister(R11),ArgConstant("1"));
                                 AsmAdd(ArgRegister(R11), ArgConstantMemory("16")); (* skip over the GC word when indexing *)
                                 AsmAdd(ArgRegister(R11), ArgRegister(RAX))]
                                in
                              let jump_condition1 = fresh_name "name_equal" in
                              let jump_condition2 = fresh_name "name_equal" in
                              let condition1 = [AsmMov(ArgRegister(R9), arg1); AsmSub(ArgRegister(R9),ArgConstant("1")); AsmMov(ArgRegister(R9), ArgMemory(AddrByRegister(R9)));
                              AsmCmp(ArgRegister(R9), ArgRegister(RAX)); AsmJg(jump_condition1)] in
                              let condition2 = [AsmCmp(ArgRegister(RAX), ArgConstant("0")); AsmJge(jump_condition2)] in
                              let error_message = [AsmMov(ArgRegister(RDI),ArgConstant("4")); AsmCall("stopWithError")] in

                              compile_expression env expr1 @
                              error_checking1 e @
                              save_number1 @
                              
                              compile_expression env' expr2 @
                              error_checking2 e @
                              [AsmSar(ArgRegister(RAX), ArgConstant("1"))] @
                              [AsmMov(ArgRegister(R11), ArgRegister(RAX))] @
                              condition1 @ error_message @ [AsmLabel(jump_condition1)] @ condition2 @ error_message @ [AsmLabel(jump_condition2)] @

                              ptr_offset @
                              [AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(R11)))]
                          )
                        | EIf (ex1, ex2, ex3) ->
                          let name_end_if = fresh_name "name_end_if" in
                          let name_false = fresh_name "if_false" in
                          let true_inst = [AsmJne(name_false)] @ compile_expression env ex2 @ [AsmJmp( name_end_if )] in 
                          let false_inst = [AsmLabel(name_false)] @ compile_expression env ex3 @ [AsmLabel(name_end_if)] in
                          let comparison = compile_expression env ex1 @ error_checking1(e) @ 
                                           [AsmMov(ArgRegister(R10), ArgConstant("0xFFFFFFFFFFFFFFFF"))] @
                                           [AsmCmp(ArgRegister(RAX), ArgRegister(R10))] in
                          comparison @ true_inst @ false_inst
                        (* | ECall (name, expr_list) -> (*caller perspective*)

                          (* let save_registers = [AsmPush(ArgRegister(RAX))] in  *)
                          let pushing_arguments_reverse = 

                            let rec reverse_push (expr_list: expr list) (n : int)  : instruction list =
                              (match expr_list with 
                               | [] -> []
                               | element :: lst -> 
                                 let arg = compile_expression env element in
                                 arg @ [AsmMov(ArgMemory(AddrByRegisterOffset(RSP,n*8)), ArgRegister(RAX))] @ reverse_push lst (n+1) 
                              )
                            in
                            let len = (List.length expr_list) in
                            [AsmAdd(ArgRegister(RSP), ArgConstant(string_of_int(-(len)*8)))] @ reverse_push expr_list 0
                          in 
                          let call = [AsmCall(name)] in 
                          let remove_args = 
                            let n = List.length expr_list * 8 in 
                            [AsmAdd(ArgRegister(RSP),ArgConstant(string_of_int(n)))]
                          in 
                          (* let restore_caller_saved = [AsmPop(ArgRegister(RAX))] in *)
                          (*save_registers @*) pushing_arguments_reverse @ call @ remove_args (*@ restore_caller_saved*)
                           *)

                        (* TODO: unsure if heap_cursor should be saved as a bird pointer *)

                          | ESet (ptr, index, value) -> 
                            compile_expression env ptr @
                            error_checking1 e @
                            let arg1, env' = allocate_temp_variable env in
                            let save_number1 = [AsmMov(arg1, ArgRegister(RAX))] in (* arg1 contains ptr; arg2 contains index *)
                            let arg2, env'' = allocate_temp_variable env' in
                            let save_number2 = [AsmMov(arg2, ArgRegister(RAX))] in
                            let inbound = 
                              let jump_condition1 = fresh_name "name_equal" in
                              let jump_condition2 = fresh_name "name_equal" in
                              let condition1 = [AsmMov(ArgRegister(R9), arg1); AsmSub(ArgRegister(R9), ArgConstantMemory("1")); AsmMov(ArgRegister(R9), ArgMemory(AddrByRegister(R9))); AsmCmp(ArgRegister(R9), ArgRegister(RAX)); AsmJge(jump_condition1)] in
                              let condition2 = [AsmCmp(ArgRegister(RAX), ArgConstant("0")); AsmJge(jump_condition2)] in
                              let error_message = [AsmMov(ArgRegister(RDI),ArgConstant("4")); AsmCall("stopWithError")] in

                              [AsmSar(ArgRegister(RAX), ArgConstant("1"))] @
                              [AsmMov(ArgRegister(R11), ArgRegister(RAX))] @
                              condition1 @ error_message @ [AsmLabel(jump_condition1)] @ condition2 @ error_message @ [AsmLabel(jump_condition2)]
                            in
                            save_number1 @
                            compile_expression env'' index @
                            error_checking2 e @
                            save_number2 @
                            inbound @
                            compile_expression env'' value @ (* value is in RAX *)
                            let me_touch_ur_balls = [AsmMov(ArgRegister(R8), arg1); (* move ptr into R8 *)
                                                     AsmSub(ArgRegister(R8), ArgConstantMemory("1")); (* make into machine ptr *)
                                                     AsmMov(ArgRegister(R9), arg2); (* move index into R9 *)
                                                     AsmSar(ArgRegister(R9), ArgConstant("1")); (* convert bird number to machine number *)
                                                     AsmIMul(ArgRegister(R9), ArgConstant("8")); (* multiply index by 8 to get into bits *)
                                                     AsmAdd(ArgRegister(R9), ArgConstant("16"));(* taking into consideration the GC word*)
                                                     AsmAdd(ArgRegister(R8), ArgRegister(R9)); (* R8 contains pointer to index in tuple *)
                                                     AsmMov(ArgMemory(AddrByRegister(R8)), ArgRegister(RAX)); (* move value into that index *)
                                                     ] in  
                          me_touch_ur_balls @
                          [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                          [AsmMov(arg1, ArgRegister(R10))] @ (* zero this 9a7ba out *)
                          [AsmMov(arg2, ArgRegister(R10))] (* zero this 9a7ba out *)

                        | ETuple (expr_list) ->
                          let size = (2 + List.length expr_list) * 8 in (* adding gc word bit *)
                          let no_gc = fresh_name "no_gc" in
                          let compare_to_end_heap = [AsmMov(ArgRegister(R10), ArgConstant(string_of_int(size)));
                                                   AsmMov(ArgRegister(R9),ArgMemory(AddrByLabel("heap_cursor")));
                                                   AsmAdd(ArgRegister(R9),ArgRegister(R10));
                                                   AsmCmp(ArgRegister(R9),ArgMemory(AddrByLabel("end_of_heap")));
                                                   AsmJle(no_gc);
                                                   ] 
                        in
                          let num_elements = [AsmMov(ArgRegister(R9), ArgMemory(AddrByLabel("heap_cursor")));
                                              AsmMov(ArgRegister(R10), ArgConstant(string_of_int(List.length expr_list)));
                                              AsmMov(ArgMemory(AddrByRegister(R9)), ArgRegister(R10));
                                              AsmMov(ArgRegister(R10), ArgConstant("0"));
                                              AsmAdd(ArgRegister(R9), ArgConstant("8"));
                                              AsmMov(ArgMemory(AddrByRegister(R9)), ArgRegister(R10)) (* add GC word as 0 *)
                                              ]
                                             in (*should be add and use register to hold the  + value*)
                          let arg1, env' = allocate_temp_variable env in
                          let save_return_ptr = [AsmMov(ArgRegister(R9), ArgMemory(AddrByLabel("heap_cursor")));
                                                 AsmMov(arg1, ArgRegister(R9))] in
                          let shift_heap_ptr = [AsmMov(ArgRegister(R9), ArgMemory(AddrByLabel("heap_cursor")));
                                                AsmMov(ArgRegister(R10), ArgConstant(string_of_int size));
                                                AsmAdd(ArgRegister(R9), ArgRegister(R10));
                                                AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(R9))] in (*use a register instead*)
                          let rec loop (i : expr list) (index : int) =
                            match i with 
                            | [] -> []
                            | element :: lst -> 
                              compile_expression env' element @
                              error_checking1 e @
                              let ptr_offset = [(AsmMov(ArgRegister(R10),ArgConstant(string_of_int(index * 8))));
                                                AsmMov(ArgRegister(R11), arg1);
                                                AsmAdd(ArgRegister(R11),ArgRegister(R10)) ] in
                              ptr_offset @ [AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister(RAX))] @
                              loop lst (index + 1)
                          in
                          compare_to_end_heap @ call_gc @ [AsmLabel(no_gc)] @save_return_ptr @ num_elements @ shift_heap_ptr @ loop expr_list 2 @
                          [AsmMov(ArgRegister(RAX), arg1)] @ [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                          [AsmMov(arg1, ArgRegister(R10))] (* zero this 9a7ba out *) @
                          [AsmAdd(ArgRegister(RAX), ArgConstant("1"))]
                        
                        | EAppl (closure, arg, tail) ->
                          if tail = false then
                            (
                            compile_expression env closure @ 
                            error_checking1 e @
                            let arg1, env' = allocate_temp_variable env in
                            let save_number1 = [AsmMov(arg1, ArgRegister(RAX))] in
                            let arg2, _ = allocate_temp_variable env' in
                            let save_number2 = [AsmMov(arg2, ArgRegister(RAX))] in
                            save_number1 @
                            compile_expression env' arg @
                            save_number2 @
                            let closure_label = fresh_name "closure_label" in
                            let done_with_function = fresh_name "done_with_function" in
                            let compare = 
                              [AsmMov(ArgRegister(R9), arg1);
                              AsmSub(ArgRegister(R9), ArgConstant("1"));
                              AsmMov(ArgRegister(R11), ArgMemory(AddrByRegister(R9))); (* copy R9 *)
                              AsmMov(ArgRegister(R8), ArgConstant("0x8000000000000000")); (* remove first 1 *)
                              AsmSub(ArgRegister(R11), ArgRegister(R8));(* R11 holds the number of args *)
                              AsmAdd(ArgRegister(R11), ArgConstant("1"));
                              AsmAdd(ArgRegister(R9), ArgConstant("16")); (* to count for the GC word *)
                              AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(R9)));
                              AsmCmp(ArgRegister(R10), ArgRegister(R11));
                              AsmJg(closure_label);
                              ] 
                            in

                            let no_gc = fresh_name "no_gc" 
                            in
                            let check_for_gc = [AsmAdd(ArgRegister(R11),ArgConstant("4"));
                                                AsmIMul(ArgRegister(R11),ArgConstant("8"));(*R11 holds the needed size*)
                                                AsmMov(ArgRegister(R10),ArgMemory(AddrByLabel("heap_cursor")));
                                                AsmAdd(ArgRegister(R10),ArgRegister(R11));
                                                AsmCmp(ArgRegister(R10),ArgMemory(AddrByLabel("end_of_heap")));
                                                AsmJle(no_gc);        
                                                AsmMov(ArgRegister(R10),ArgRegister(R11)) (*because we hold the number of needed args in R10 for our calling function*)                                        

                            ]
                            in  

                            (* assume that not all args are provided *)
  
                            let copy = [AsmMov(ArgRegister(RSI), arg1); AsmSub(ArgRegister(RSI), ArgConstant("1")); AsmMov(ArgRegister(RDI), ArgMemory(AddrByLabel("heap_cursor"))); 
                                        AsmMov(ArgRegister(RCX), ArgMemory(AddrByRegister(RSI))); 
                                        AsmMov(ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF")); 
                                        AsmAnd(ArgRegister(RCX), ArgRegister(R10));
                                        AsmAdd(ArgRegister(RCX), ArgConstant("4"));(*made it 4 to count for the GC word, not really sure what this line was doing*)
                                        AsmMov(arg2,ArgRegister(RCX));
                                        AsmRepMovsq;
                                        AsmMov(ArgRegister(R9), ArgMemory(AddrByLabel("heap_cursor")));
                                        AsmMov(ArgRegister(RCX), arg2);
                                        AsmMov(ArgRegister(RDI), ArgMemory(AddrByLabel("heap_cursor")))] in
                            let move_heap_cursor = [
                                                  AsmIMul(ArgRegister(RCX), ArgConstant("8"));
                                                  AsmAdd(ArgRegister(RCX), ArgConstant("8"));
                                                  AsmAdd(ArgRegister(R9), ArgRegister(RCX));
                                                  AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(R9))] in
                            let add_to_end_list = [AsmMov(ArgRegister(R11), ArgMemory(AddrByLabel("heap_cursor")));
                                                  AsmSub(ArgRegister(R11), ArgConstant("8"));
                                                  AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister(RAX))] in (* assuming that this happens after move heap_cursor *)
                            let increase_args_by_1 = [AsmMov(ArgRegister(R10), ArgConstant("1")); AsmAdd(ArgMemory(AddrByRegister(RDI)), ArgRegister(R10))] in  
  
                            let call_function = [AsmMov(ArgRegister(RSI), arg1); 
                                                AsmSub(ArgRegister(RSI), ArgConstant("1"));
                                                AsmMov(ArgRegister(R9), ArgRegister(RSI));
                                                AsmMov(ArgRegister(RCX), ArgMemory(AddrByRegister(R9)));
                                                AsmMov(ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF")); (*number of current args in RCX*)
                                                AsmAnd(ArgRegister(RCX), ArgRegister(R10));
                                                AsmAdd(ArgRegister(R9), ArgConstant("16"));(*counting for the GC word + first arg*)
                                                AsmMov(ArgRegister(R9), ArgMemory(AddrByRegister(R9)));(*R9 holds the max number of args*)
                                                AsmIMul(ArgRegister(R9), ArgConstant("8"));
                                                AsmSub(ArgRegister(RSP), ArgRegister(R9));
                                                AsmMov(ArgRegister(RDI), ArgRegister(RSP));
                                                AsmAdd(ArgRegister(RSI),ArgConstant("32"));(*mmaybe ? YES*)
                                                AsmRepMovsq;
                                                AsmMov(ArgMemory(AddrByRegister(RDI)), ArgRegister(RAX));
                                                AsmMov(ArgRegister(R11), arg1);
                                                AsmSub(ArgRegister(R11), ArgConstant("1"));
                                                AsmAdd(ArgRegister(R11), ArgConstant("24")); (*address of the function*)
                                                AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(R11)));
                                                AsmCall("RAX");
                                                AsmMov(ArgRegister(R11), arg1);
                                                AsmSub(ArgRegister(R11), ArgConstant("1"));
                                                AsmAdd(ArgRegister(R11), ArgConstant("16")); (*counting for GC word*)
                                                AsmMov(ArgRegister(R11), ArgMemory(AddrByRegister(R11)));
                                                AsmIMul(ArgRegister(R11), ArgConstant("8"));
                                                AsmAdd(ArgRegister(RSP), ArgRegister(R11)); (*tearing down stack*)
                                                AsmJmp(done_with_function)
                                                ] in
                            let create_new_closure = copy @ move_heap_cursor @ add_to_end_list @ increase_args_by_1 @ 
                            [AsmMov(ArgRegister(RAX), ArgRegister(RDI)); AsmAdd(ArgRegister(RAX), ArgConstant("1"))] in
                            
                            compare @ 

                            call_function @
                            [AsmLabel(closure_label)] @
                            check_for_gc @ (* we need to think if we need to save anything on the stack before calling the garbage collector*)
                            call_gc @
                            [AsmLabel(no_gc)]@
                            create_new_closure @

                            [AsmLabel(done_with_function)] @
                            [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                            [AsmMov(arg1, ArgRegister(R10))] @ (* zero this 9a7ba out *)
                            [AsmMov(arg2, ArgRegister(R10))] (* zero this 9a7ba out *)
                          )



                          else
                            begin
                              compile_expression env closure @ 
                              error_checking1 e @
                              let arg1, env' = allocate_temp_variable env in
                              let save_number1 = [AsmMov(arg1, ArgRegister(RAX))] in
                              let arg2, env'' = allocate_temp_variable env' in
                              let save_number2 = [AsmMov(arg2, ArgRegister(RAX))] in
                              save_number1 @
                              compile_expression env' arg @
                              save_number2 @
                              let closure_label = fresh_name "closure_label" in
                              let done_with_function = fresh_name "done_with_function" in
                              let compare = 
                                [AsmMov(ArgRegister(R9), arg1);
                                AsmSub(ArgRegister(R9), ArgConstant("1"));
                                AsmMov(ArgRegister(R11), ArgMemory(AddrByRegister(R9))); (* copy R9 *)
                                AsmMov(ArgRegister(R8), ArgConstant("0x8000000000000000")); (* remove first 1 *)
                                AsmSub(ArgRegister(R11), ArgRegister(R8));(* R11 holds the number of args *)
                                AsmAdd(ArgRegister(R11), ArgConstant("1"));
                                AsmAdd(ArgRegister(R9), ArgConstant("16")); (* to count for the GC word *)
                                AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(R9)));
                                AsmCmp(ArgRegister(R10), ArgRegister(R11));
                                AsmJg(closure_label);
                                ] 
                              in

                              let number_of_caller_args = find_named_variable "parameters" env'' in
                              let number_of_args_in_R11_caller = [AsmMov(ArgRegister(R11), number_of_caller_args)] in

                              let number_of_args_in_R10_callee = 
                                [AsmMov(ArgRegister(R9), arg2);
                                AsmSub(ArgRegister(R9), ArgConstant("1"));
                                AsmAdd(ArgRegister(R9),ArgConstant("16"));
                                AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(R9)))]
                              in

                              let regular_call = fresh_name "regular_call" in

                              let compare_number_param = 
                                [AsmCmp(ArgRegister(R11), ArgRegister(R10));
                                AsmJl(regular_call)]
                              in

                              let glue1 = number_of_args_in_R11_caller @ number_of_args_in_R10_callee @ compare_number_param 
                              in

                              let no_gc = fresh_name "no_gc"
                              in

                              let check_for_gc = [AsmAdd(ArgRegister(R11),ArgConstant("4"));
                                                  AsmIMul(ArgRegister(R11),ArgConstant("8"));(*R11 holds the needed size*)
                                                  AsmMov(ArgRegister(R10),ArgMemory(AddrByLabel("heap_cursor")));
                                                  AsmAdd(ArgRegister(R10),ArgRegister(R11));
                                                  AsmCmp(ArgRegister(R10),ArgMemory(AddrByLabel("end_of_heap")));
                                                  AsmJle(no_gc);        
                                                  AsmMov(ArgRegister(R10),ArgRegister(R11)) (*because we hold the number of needed args in R10 for our calling function*)
                              ]
                              in

                              (* assume that not all args are provided *)
                              let copy = [AsmMov(ArgRegister(RSI), arg1); AsmSub(ArgRegister(RSI), ArgConstant("1")); AsmMov(ArgRegister(RDI), ArgMemory(AddrByLabel("heap_cursor"))); 
                                        AsmMov(ArgRegister(RCX), ArgMemory(AddrByRegister(RSI))); 
                                        AsmMov(ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF")); 
                                        AsmAnd(ArgRegister(RCX), ArgRegister(R10));
                                        AsmAdd(ArgRegister(RCX), ArgConstant("4"));(*made it 4 to count for the GC word, not really sure what this line was doing*)
                                        AsmMov(arg2,ArgRegister(RCX));
                                        AsmRepMovsq;
                                        AsmMov(ArgRegister(R9), ArgMemory(AddrByLabel("heap_cursor")));
                                        AsmMov(ArgRegister(RCX), arg2);
                                        AsmMov(ArgRegister(RDI), ArgMemory(AddrByLabel("heap_cursor")))] in
                            let move_heap_cursor = [
                                                  AsmIMul(ArgRegister(RCX), ArgConstant("8"));
                                                  AsmAdd(ArgRegister(RCX), ArgConstant("8"));
                                                  AsmAdd(ArgRegister(R9), ArgRegister(RCX));
                                                  AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(R9))] in
                            let add_to_end_list = [AsmMov(ArgRegister(R11), ArgMemory(AddrByLabel("heap_cursor")));
                                                  AsmSub(ArgRegister(R11), ArgConstant("8"));
                                                  AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister(RAX))] in (* assuming that this happens after move heap_cursor *)
                            let increase_args_by_1 = [AsmMov(ArgRegister(R10), ArgConstant("1")); AsmAdd(ArgMemory(AddrByRegister(RDI)), ArgRegister(R10))] in  
  
                            let call_function = [AsmMov(ArgRegister(RSI), arg1); 
                                                AsmSub(ArgRegister(RSI), ArgConstant("1"));
                                                AsmMov(ArgRegister(R9), ArgRegister(RSI));
                                                AsmMov(ArgRegister(RCX), ArgMemory(AddrByRegister(R9)));
                                                AsmMov(ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF")); (*number of current args in RCX*)
                                                AsmAnd(ArgRegister(RCX), ArgRegister(R10));
                                                AsmAdd(ArgRegister(R9), ArgConstant("16"));(*counting for the GC word + first arg*)
                                                AsmMov(ArgRegister(R9), ArgMemory(AddrByRegister(R9)));(*R9 holds the max number of args*)
                                                AsmIMul(ArgRegister(R9), ArgConstant("8"));
                                                AsmSub(ArgRegister(RSP), ArgRegister(R9));
                                                AsmMov(ArgRegister(RDI), ArgRegister(RSP));
                                                AsmAdd(ArgRegister(RSI),ArgConstant("32"));(*mmaybe ? YES*)
                                                AsmRepMovsq;
                                                AsmMov(ArgMemory(AddrByRegister(RDI)), ArgRegister(RAX));
                                                AsmMov(ArgRegister(R11), arg1);
                                                AsmSub(ArgRegister(R11), ArgConstant("1"));
                                                AsmAdd(ArgRegister(R11), ArgConstant("24")); (*address of the function*)
                                                AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(R11)));
                                                AsmCall("RAX");
                                                AsmMov(ArgRegister(R11), arg1);
                                                AsmSub(ArgRegister(R11), ArgConstant("1"));
                                                AsmAdd(ArgRegister(R11), ArgConstant("16")); (*counting for GC word*)
                                                AsmMov(ArgRegister(R11), ArgMemory(AddrByRegister(R11)));
                                                AsmIMul(ArgRegister(R11), ArgConstant("8"));
                                                AsmAdd(ArgRegister(RSP), ArgRegister(R11)); (*tearing down stack*)
                                                AsmJmp(done_with_function)
                                                ] in
                            let create_new_closure = copy @ move_heap_cursor @ add_to_end_list @ increase_args_by_1 @ 
                            [AsmMov(ArgRegister(RAX), ArgRegister(RDI)); AsmAdd(ArgRegister(RAX), ArgConstant("1"))] in

    
                              let tail_call_function = [AsmMov(ArgRegister(RSI), arg1); 
                                                  AsmSub(ArgRegister(RSI), ArgConstant("1"));
                                                  AsmMov(ArgRegister(R9), ArgRegister(RSI));
                                                  AsmMov(ArgRegister(RCX), ArgMemory(AddrByRegister(R9)));
                                                  AsmMov(ArgRegister(R10), ArgConstant("0x7FFFFFFFFFFFFFFF")); (* number of current args in RCX *)
                                                  AsmAnd(ArgRegister(RCX), ArgRegister(R10));
                                                  AsmAdd(ArgRegister(R9), ArgConstant("16")); (* counting for the GC word + first arg *)
                                                  AsmMov(ArgRegister(R9), ArgMemory(AddrByRegister(R9))); (* R9 holds the max number of args *)
                                                  AsmIMul(ArgRegister(R9), ArgConstant("8"));
                                                  AsmSub(ArgRegister(RSP), ArgRegister(R9));
                                                  AsmMov(ArgRegister(RDI), ArgRegister(RBP));
                                                  AsmAdd(ArgRegister(RDI), ArgConstant("16")); (* new destination *)
                                                  AsmAdd(ArgRegister(RSI),ArgConstant("32"));(* maybe ? YES *)
                                                  AsmRepMovsq;
                                                  AsmMov(ArgMemory(AddrByRegister(RDI)), ArgRegister(RAX));
                                                  AsmMov(ArgRegister(R11), arg1);
                                                  AsmSub(ArgRegister(R11), ArgConstant("1"));
                                                  AsmAdd(ArgRegister(R11), ArgConstant("24")); (*address of the function*)
                                                  AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(R11)));
                                                  AsmMov(ArgRegister(RBP), ArgRegister(RSP)); (* tear down the stack *)
                                                  AsmPop(ArgRegister(RBP));
                                                  AsmJmp("RAX"); (* jump *)
                                                  AsmMov(ArgRegister(R11), number_of_caller_args);
                                                  AsmAdd(ArgRegister(RSP), ArgRegister(R11)); (*tearing down stack*)
                                                  AsmJmp(done_with_function)
                                                  ] in
                              
                              compare @ 
                              glue1 @
                              tail_call_function @
                              [AsmLabel(regular_call)] @
                              call_function @
                              [AsmLabel(closure_label)] @
                              check_for_gc @ (* we need to think if we need to save anything on the stack before calling the garbage collector*)
                              call_gc @
                              [AsmLabel(no_gc)]@
                              create_new_closure @


                              [AsmLabel(done_with_function)] @
                              [AsmMov(ArgRegister(R10), ArgConstant("0"))] @
                              [AsmMov(arg1, ArgRegister(R10))] @ (* zero this 9a7ba out *)
                              [AsmMov(arg2, ArgRegister(R10))] (* zero this 9a7ba out *)
                            end
                        ;;
                          


                          let stack_memory_of_argument (argument : argument) : int = 
                            (match argument with
                             | ArgConstant _ -> 0
                             | ArgRegister _ -> 0
                             | ArgConstantMemory _ -> 0
                             | ArgMemory address -> 
                               (match address with
                                | AddrByRegister _ -> 0
                                | AddrByRegisterOffset (_, n) -> -n
                                | AddrByLabel _ -> 0
                                | AddressByRegisterProductOffset (_, _, _) -> 0 (* TODO: come back to this *)
                               )
                              | ArgLabelOffset _ -> 0
                            )
                             ;;
                            


                            let stack_memory_of_instruction (instruction : instruction) : int = 
                              (match instruction with
                               | AsmAdd (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmIMul (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmMov (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmSub (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmShl (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmShr (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmSal (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmSar (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmAnd (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmOr (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmXor (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmLabel _ -> 0
                               | AsmCmp (a, b) -> max (stack_memory_of_argument a) (stack_memory_of_argument b)
                               | AsmJmp _ -> 0
                               | AsmJe _ -> 0
                               | AsmJne _ -> 0
                               | AsmRet -> 0
                               | AsmJle _ -> 0
                               | AsmJl _ -> 0
                               | AsmJge _ -> 0
                               | AsmJg _ -> 0
                               | AsmPush _ -> 0
                               | AsmPop _ -> 0
                               | AsmCall _ -> 0
                               | AsmSection _ -> 0
                               | AsmDq _ -> 0
                               | AsmAlign _ -> 0
                               | AsmRepMovsq -> 0
                               | AsmRepStosq -> 0
                              )
                            ;;
                              


                              let stack_memory_of_instruction_list (instruction_list : instruction list) : int = 
                                let int_list = List.map stack_memory_of_instruction instruction_list in
                                let rec loop (int_list : int list)(n : int) : int =
                                  match int_list with
                                  | [] -> n
                                  | element :: list -> 
                                    if element > n then
                                      loop list element
                                    else
                                      loop list n
                                in
                                loop int_list 0
                              ;;

                                let compile_function (env : environment) (declaration : declaration) :  instruction list = (*callee perspective*) (*we need to create an environment that knows where variable are, we need to setup local variables not through the string list*)
                                  let DFunction(name, parameter_list, expr') = declaration in 
                                  let expr = mark_tail expr' in
                                  let name_f = [(AsmLabel(name))] in 
                                  let env'' = allocate_named_list_of_variables parameter_list env in
                                  let env' = allocate_number_of_args parameter_list env'' in
                                  let actual_work = compile_expression env' expr in
                                  let offset = stack_memory_of_instruction_list actual_work in
                                  let stack_setup = 
                                    [AsmPush(ArgRegister(RBP));
                                     AsmMov(ArgRegister(RBP), ArgRegister(RSP));
                                     AsmSub(ArgRegister(RSP), ArgConstant(string_of_int offset));
                                     (* add stonks here *)
                                     AsmMov(ArgRegister(RAX), ArgConstant("0"));
                                     AsmMov(ArgRegister(RDI), ArgRegister(RSP));
                                     AsmMov(ArgRegister(RCX), ArgConstant(string_of_int offset));
                                     AsmSar(ArgRegister(RCX), ArgConstant("3"));
                                     AsmRepStosq] in

                                  let stack_tear = [AsmMov(ArgRegister(RSP),ArgRegister(RBP));
                                                    AsmPop(ArgRegister(RBP));
                                                    AsmRet] in 
                                  name_f @ stack_setup @ actual_work @ stack_tear
                                  ;;

                                  let generate_env_fns (decs : declaration list) : environment =
                                    let env = empty_environment in
                                    let rec loop decs env =
                                      match decs with 
                                      | [] -> env
                                      | element :: decs' -> 
                                        let DFunction(name, _, _) = element in
                                        let env' = allocate_closure name env in
                                        loop decs' env'
                                    in
                                    loop decs env
                                  ;;

                                  let compile_program (program : program) : instruction list * instruction list =
                                    (* let program = mark_tail program1 in *)
                                    let Program(decs, e) = program in 
                                    let env = generate_env_fns decs in
                                    let list_of_lists = List.map (compile_function env) decs in
                                    let rec loop (list_of_lists : instruction list list) : instruction list =
                                      match list_of_lists with 
                                      | []-> []
                                      | list::list_list -> 
                                        list @ loop(list_list) 
                                    in
                                    let instruction_list_functions = loop(list_of_lists) in

                                    let instructions = compile_expression env e in
                                    let max_offset = stack_memory_of_instruction_list instructions in
                                    
                                    let main_instructions = instruction_list_functions @
                                    [AsmLabel("bird_main")] @
                                    [AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(RDI))] @
                                    [AsmMov(ArgMemory(AddrByLabel("start_of_heap")), ArgRegister(RDI))] @
                                    [AsmMov(ArgMemory(AddrByLabel("end_of_heap")), ArgRegister(RSI))] @                                    
                                    [AsmPush (ArgRegister (RBP))] @
                                    [AsmMov(ArgMemory(AddrByLabel("start_of_stack")), ArgRegister(RBP))] @
                                    
                                    (* TODO: call right before garbage collection *)
                                    [AsmMov(ArgMemory(AddrByLabel("end_of_stack")), ArgRegister(RSI))] @

                                    [AsmMov (ArgRegister (RBP), ArgRegister(RSP))] @
                                    [AsmSub (ArgRegister(RSP), ArgConstant (string_of_int (max_offset)))] @
                                    [AsmMov(ArgRegister(RAX), ArgConstant("0"))] @
                                    [AsmMov(ArgRegister(RDI), ArgRegister(RSP))] @
                                    [AsmMov(ArgRegister(RCX), ArgConstant (string_of_int (max_offset)))] @
                                    [AsmSar(ArgRegister(RCX), ArgConstant("3"))] @
                                    [AsmRepStosq] @
                                    instructions @ 
                                    [AsmMov (ArgRegister (RSP), ArgRegister(RBP))] @
                                    [AsmPop (ArgRegister (RBP))] @
                                    [AsmRet]
                                    in
                                    let global_closures = List.concat (List.map generate_closure decs) in
                                    main_instructions, global_closures
                                  ;;
                                    

                                    let compile_to_assembly_code (program : program) : string =
                                      let _= Wellformedness.check_well_formed program in
                                      let instructions, global_variables = compile_program program in
                                      let global_code = code_of_instruction_list global_variables in
                                      let instruction_code = code_of_instruction_list instructions in
                                      let section_data = code_of_instruction_list ([AsmSection ("data");
                                                                                    AsmAlign ("8")]) in
                                      (* let heap_cursor = code_of_address (AddrByLabel ("heap_cursor")) in  *)
                                      (* let dq = code_of_instruction (AsmDq(ArgMemory(AddrByRegister(RDI)))) in *)
                                      section_data ^
                                      "heap_cursor:\n" ^
                                      "  dq 0\n" ^
                                      "align 8\n" ^
                                      "start_of_stack:\n" ^
                                      "  dq 0\n" ^
                                      "align 8\n" ^
                                      "end_of_stack:\n" ^
                                      "  dq 0\n" ^
                                      "align 8\n" ^
                                      "start_of_heap:\n" ^
                                      "  dq 0\n" ^
                                      "align 8\n" ^
                                      "end_of_heap:\n" ^
                                      "  dq 0\n" ^
                                      global_code ^

                                      "section .text\n" ^
                                      "global bird_main\n" ^
                                      "global start_of_heap\n" ^
                                      "global end_of_heap\n" ^
                                      "global start_of_stack\n" ^
                                      "global end_of_stack\n" ^
                                      "global heap_cursor\n" ^
                                      "extern stopWithError\n" ^
                                      "extern printValue\n" ^
                                      "extern gc\n" ^

                                      instruction_code ^
                                      "\n"
                                    ;;

