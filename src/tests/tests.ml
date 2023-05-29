(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;
open HatchCompiler;;
open Assembly;;

let test_code_of_register_1 =
  "testing string of register RAX" >:: fun _ ->
    assert_equal ("RAX") ( code_of_register RAX)
;;

let test_code_of_register_2 =
  "testing string of register RSP" >:: fun _ ->
    assert_equal ("RSP") (code_of_register RSP)
;;

let test_code_of_address_1 =
  "testing string of code of address" >:: fun _ ->
    assert_equal ("[RSP]") (code_of_address (AddrByRegister(RSP)))
;;
let test_code_of_address_2 =
  "testing string of code of address with offset" >:: fun _ ->
    assert_equal ("[RSP-8]") (code_of_address( AddrByRegisterOffset(RSP, 8)))
;;

let test_code_of_argument_1 =
  "testing string argument" >:: fun _ ->
    assert_equal ("[RAX-8]") (code_of_argument(ArgMemory(AddrByRegisterOffset(RAX,8))))
;;

let test_code_of_argument_2 =
  "testing string argument 2" >:: fun _ ->
    assert_equal ("0xFFFFFFFFFFFFFFFE") (code_of_argument(ArgConstant("0xFFFFFFFFFFFFFFFE")))
;;
let test_code_of_instruction_1 =
  "testing string of intructions" >:: fun _ ->
    assert_equal ("  imul RAX,[RSP-8]") (code_of_instruction ((AsmIMul(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(RSP,8))))))
;;
let test_code_of_instruction_2 =
  "testing string of intructions" >:: fun _ ->
    assert_equal ("  sub RSP,RAX") (code_of_instruction (AsmSub(ArgRegister(RSP),ArgRegister(RAX) )) )
;;

let test_code_of_intruction_list_1 = 
  "testing code of instruction list" >:: fun _ -> 
    assert_equal ("  imul RAX,[RSP-8]\n  sub RSP,RAX\n  ret\n") (code_of_instruction_list ([AsmIMul(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(RSP,8))) ; AsmSub(ArgRegister(RSP),ArgRegister(RAX) );AsmRet]))
;;





let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/let_5.bird" "12";
    test_success "test_code/minus4.bird" "-4";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/minus2.bird" "-2";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/plus4.bird" "10"; 
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/plus2.bird" "6";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/less_than.bird" "false";
    test_success "test_code/less_than_true.bird" "true";
    test_success "test_code/greater_than.bird" "true";
    test_success "test_code/greater_than_false.bird" "false";
    test_success "test_code/if_statemnt.bird" "true";
    test_success "test_code/is_int.bird" "true";
    test_success "test_code/is_int_false.bird" "false";
    test_success "test_code/is_bool.bird" "true";
    test_success "test_code/is_bool_false.bird" "false";
    test_success "test_code/if_statement_easy.bird" "false";
    test_success "test_code/equal_to.bird" "true";
    test_success "test_code/equal_to2.bird" "true";
    test_success "test_code/or.bird" "true";
    test_success "test_code/or2.bird" "true";
    test_success "test_code/and.bird" "true";
    test_success "test_code/print1.bird" "1";
    test_success "test_code/print2.bird" "12\n12";
    test_success "test_code/print3.bird" "1\n4";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/print_complex.bird" "5\n2\n3";
    test_success "test_code/print_complex2.bird" "true\nfalse\nfalse";
    test_runtime_failure "test_code/error1.bird" 1;
    test_runtime_failure "test_code/error1_1.bird" 2;
    test_runtime_failure "test_code/error2_1.bird" 2;
    test_runtime_failure "test_code/error2.bird" 2;
    (* test_success "test_code/function_call1.bird" "5";
    test_success "test_code/function_call2.bird" "18";
    test_success "test_code/function_call3.bird" "15";
    test_success "test_code/function_call4.bird" "1";
    test_success "test_code/function_call5.bird" "true";
    test_compile_failure "test_code/function_error1.bird" "Function g is not defined.";
    test_compile_failure "test_code/function_error2.bird" "Function f is called with an incorrect number of arguments.";
    test_compile_failure "test_code/function_error3.bird" "Function f declares a duplicate parameter x.";
    test_compile_failure "test_code/function_error4.bird" "Duplicate definition of function f.";
    test_compile_failure "test_code/function_error5.bird" "Unbound variable y.";
    test_compile_failure "test_code/function_error6.bird" "Function f is called with an incorrect number of arguments.Function g is not defined.Unbound variable z.Unbound variable y.";
    test_success "test_code/function_factorial.bird" "24"; *)
    test_success "test_code/op_less_than.bird" "true";
    test_compile_failure "test_code/unbound_variable_x.bird" "Unbound variable x.";
    (* test_success "test_code/function_left_to_right.bird" "1\n2\n1"; *)

    test_success "test_code/tuple1.bird" "6";
    test_success "test_code/tuple2.bird" "(true, false)";
    test_success "test_code/tuple3.bird" "1\n2\n1";
    test_runtime_failure "test_code/tuple4.bird" 4;    
    test_success "test_code/tuple5.bird" "3";
    test_runtime_failure "test_code/tuple6.bird" 4;
    test_runtime_failure "test_code/tuple7.bird" 3;
    test_runtime_failure "test_code/tuple8.bird" 1;
    test_runtime_failure "test_code/tuple9.bird" 1;
    test_success "test_code/tuple10.bird" "3";
    test_runtime_failure "test_code/tuple11.bird" 1;
    test_runtime_failure "test_code/true3.bird" 1;
    test_success "test_code/tuple12.bird" "true";
    test_runtime_failure "test_code/tuple13.bird" 1;
    test_success "test_code/tuple14.bird" "8";
    test_runtime_failure "test_code/tuple15.bird" 2;
    test_success "test_code/tuple16.bird" "false";
    test_runtime_failure "test_code/tuple17.bird" 4;

    (* CLOSURE TESTS *)
    test_success "test_code/closure1.bird" "3";
    (* test_success "test_code/closure2.bird" "<closure@0000000000401150>[0/2](?, ?)"; *)
    (* test_success "test_code/closure3.bird" "<closure@0000000000401150>[2/6](1, 2, ?, ?, ?, ?)"; *)
    test_success "test_code/closure4.bird" "6";
    (* test_success "test_code/closure5.bird" "<closure@00000000004011e2>[1/2](<closure@0000000000401150>[0/1](?), ?)"; *)
    test_success "test_code/closure6.bird" "13";
    test_runtime_failure "test_code/closure7.bird" 5;
    test_runtime_failure "test_code/closure8.bird" 5;
    test_success "test_code/closure9.bird" "20";
    test_success "test_code/closure10.bird" "978";
    test_success "test_code/closure11.bird" "5";

    (* MUTATION TESTS *)
    test_success "test_code/mutation1.bird" "12";
    test_success "test_code/mutation2.bird" "(6, 69, 420)";
    test_runtime_failure "test_code/mutation3.bird" 4;
    test_runtime_failure "test_code/mutation_error_negative.bird" 4;
    test_runtime_failure "test_code/mutation_error_nonint.bird" 1;
    test_runtime_failure "test_code/mutation_error_expectedtuple.bird" 3;
    test_runtime_failure "test_code/mutation_error_expectedtuple_gotclosure.bird" 3;
    
    (* GARBAGE COLLECTING TESTS *)
    (* test_runtime_failure "test_code/gc_use_closure_memory.bird" 7;
    test_success "test_code/gc_cycle_tuple_memory.bird" "1048576";
    test_success "test_code/gc_cycle_closure_memory.bird" "1048576";
    test_runtime_failure "test_code/gc_use_tuple_memory.bird" 7;
    test_success "test_code/gc_mark_test.bird" "16"; *)

    ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;