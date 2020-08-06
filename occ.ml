let a = Codegen.Add (Num 1, Num 2)
let () = Codegen.compile @@ Lex.treenize @@ List.rev @@ Lex.tokenize @@ Sys.argv.(1)
let _ = print_string "\n"
