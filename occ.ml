let return_arg = Sys.argv.(1) |> int_of_string

let print_first () =
  print_endline ".intel_syntax noprefix";
  print_endline ".globl _main"; ()

let () = 
  print_first ();
  print_endline "_main:";
  Printf.printf "  mov rax, %d\n" return_arg;
  print_endline "  ret"
