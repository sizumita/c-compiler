(* 文字列をcharのlistにする *)
let explode s = List.init (String.length s) (String.get s)

let print_first () =
  print_endline ".intel_syntax noprefix";
  print_endline ".globl _main"; ()

let p = explode Sys.argv.(1)
let charlist_to_int lst =
  int_of_string @@ String.of_seq @@ List.to_seq lst
let rec get_integer_rest lst result =
  match lst with
  | [] -> (charlist_to_int result, [])
  | first :: rest ->
    if first = '+' || first = '-' 
      then (charlist_to_int result, first :: rest) 
      else get_integer_rest rest (result @ [first])


let rec loop lst =
  match lst with
  | [] -> ()
  | first :: rest ->
    let value, rest2 = get_integer_rest rest [] in
      match first with
      | '-' -> Printf.printf "  sub rax, %d\n" value; loop rest2
      | '+' -> Printf.printf "  add rax, %d\n" value; loop rest2
      | _ -> print_endline "予期しない文字です"

let read_first lst =
  let value, rest = get_integer_rest lst [] in
  Printf.printf "  mov rax, %d\n" value;
  rest

let () =
  print_first ();
  print_endline "_main:";
  loop @@ read_first p;
  print_endline "  ret"
