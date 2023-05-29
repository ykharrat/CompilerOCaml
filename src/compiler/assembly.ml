(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | R10
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9
  | RBP
  | R11
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddrByLabel of string
  | AddressByRegisterProductOffset of register * register * int
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgConstantMemory of string
  | ArgLabelOffset of string * string
;;


(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmShl of argument * argument
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string
  | AsmRet
  | AsmJle of string
  | AsmJl of string
  | AsmJge of string
  | AsmJg of string
  | AsmPush of argument
  | AsmPop of argument
  | AsmCall of string
  | AsmSection of string
  | AsmAlign of string
  | AsmDq of string
  | AsmRepMovsq 
  | AsmRepStosq
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  (match register with
  | RAX -> "RAX"
  | RSP -> "RSP"
  | R10 -> "R10"
  | RDI -> "RDI"
  | RSI -> "RSI"
  | RDX -> "RDX"
  | RCX -> "RCX"
  | R8 -> "R8"
  | R9 -> "R9"
  | RBP -> "RBP"
  | R11 -> "R11"
  )
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with
  | AddrByRegister reg -> "[" ^ code_of_register reg ^ "]"
  | AddrByRegisterOffset (reg, n) -> "[" ^ (code_of_register reg) ^ "+" ^ (string_of_int n) ^ "]"
  | AddrByLabel label -> "[" ^ label ^ "]"
  | AddressByRegisterProductOffset (reg1, reg2, offset) -> "[" ^ code_of_register reg1 ^ " + " ^ code_of_register reg2 ^ " * " ^ (string_of_int offset) ^ "]"
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with
  | ArgConstant s -> s
  | ArgRegister reg -> code_of_register reg
  | ArgMemory add -> code_of_address add
  | ArgConstantMemory s -> "QWORD " ^ s
  | ArgLabelOffset (label, offset) -> label ^ " + " ^ offset
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
  *)
let code_of_instruction (instruction : instruction) : string =
  match instruction with
  | AsmAdd (arg1, arg2) -> "  add " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmIMul (arg1, arg2) -> "  imul " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmMov (arg1, arg2) -> "  mov " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmSub (arg1, arg2) -> "  sub " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmShl (arg1, arg2) -> "  shl " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmShr (arg1, arg2) -> "  shr " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmSal (arg1, arg2) -> "  sal " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmSar (arg1, arg2) -> "  sar " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2) (*arithmetic*)
  | AsmAnd (arg1, arg2) -> "  and " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmOr (arg1, arg2) -> "  or " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmXor (arg1, arg2) -> "  xor " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmLabel(s) -> s ^ " :"
  | AsmCmp (arg1, arg2) ->  "  cmp " ^ (code_of_argument arg1) ^ "," ^ (code_of_argument arg2)
  | AsmJmp (s) -> "  jmp " ^ (s)
  | AsmJe(s) -> "  je " ^ (s)
  | AsmJne (s) -> "  jne " ^ (s)
  | AsmRet -> "  ret"
  | AsmJle(s) -> "  jle " ^ (s)
  | AsmJl(s) -> "  jl " ^ (s)
  | AsmJge(s) -> "  jge " ^ (s)
  | AsmJg(s) -> "  jg " ^ (s)
  | AsmPush(s) -> "  push " ^ (code_of_argument s)
  | AsmPop(s) -> "  pop " ^ (code_of_argument s)
  | AsmCall(s) -> "  call " ^ (s)
  | AsmSection(s) -> "section ." ^ s
  | AsmAlign(s) -> "align " ^ s
  | AsmDq(s) -> "  dq " ^ s
  | AsmRepMovsq -> "  rep movsq"
  | AsmRepStosq -> "  rep stosq"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec code_of_instruction_list (instruction_list : instruction list) : string =
  match instruction_list with
  | [] -> ""
  | element :: instruction_list' -> 
    (code_of_instruction element)  ^ "\n" ^ (code_of_instruction_list instruction_list')
;;