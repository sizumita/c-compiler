open Printf

type expr =
  | Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Error of string

let print_pop () =
  print_endline "  pop rdi";
  print_endline "  pop rax"

let do_symbol symbol = printf "  %s rax, rdi\n" symbol
let push () = print_endline "  push rax"

let rec gen epr =
  match epr with
  | Num x -> printf "  push %d\n" x
  | Add (x, y) -> next x y; print_pop (); do_symbol "add"; push ()
  | Sub (x, y) -> next x y; print_pop (); do_symbol "sub"; push ()
  | Mul (x, y) -> next x y; print_pop (); do_symbol "imul"; push ()
  | Div (x, y) -> next x y; print_pop (); print_string "  cqo\n  idiv rdi\n"; push ()
  | Error s -> print_endline s
and next x y = gen x; gen y

let compile t =
  print_endline ".intel_syntax noprefix";
  print_endline ".globl _main";
  print_endline "_main:";
  gen t;
  print_endline "  pop rax";
  print_endline "  ret";
