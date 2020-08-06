let a = Codegen.Add (Num 1, Num 2)
let () = print_endline "start"
let () = Codegen.compile @@ Lex.treenize @@ Lex.tokenize @@ Sys.argv.(1)
let _ = print_string "\n"
