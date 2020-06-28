(* 文字列をcharのlistにする *)
let explode s = List.init (String.length s) (String.get s)

let print_first () =
  print_endline ".intel_syntax noprefix";
  print_endline ".globl _main"; ()

let p = explode Sys.argv.(1)

let rec is_integer chr =
  let i = int_of_char chr in
  if 48 <= i && i <= 57 
    then true 
    else false

type expr =
  | Symbol of char
  | NUM of int
  | EOF
  | ERROR of string

let int_of_char_list lst =
  int_of_string @@ String.of_seq @@ List.to_seq lst

let rec get_integer_rest lst result =
  match lst with
  | [] -> (int_of_char_list result, [])
  | first :: rest ->
    if first = ' ' then get_integer_rest rest result else
    if is_integer first
      then get_integer_rest rest (result @ [first])
      else (int_of_char_list result, first :: rest)
 

let new_token lst prev =
  match prev with
  | Symbol x -> 
    let value, rest = get_integer_rest lst [] in
    (NUM value, rest)
  | _ -> (ERROR "無効なトークンです。", [])


let rec tokenize lst is_first =
  match lst with
  | [] -> [EOF]
  | first :: rest ->
  if first = ' '
    then tokenize rest is_first
  else if is_first
    then let token, rest' = get_integer_rest (first :: rest) [] in
      (NUM token) :: tokenize rest' false 
  else if first = '-' || first = '+' 
    then 
      let token, rest' = new_token rest (Symbol first) in
      (Symbol first) :: token :: tokenize rest' false
  else (ERROR "無効なトークンです。") :: tokenize [] false

let print_sub i = Printf.printf "  sub rax, %d\n" i
let print_add i = Printf.printf "  add rax, %d\n" i

let rec read_tokens tokens =
  match tokens with
  | (NUM x) :: rest -> Printf.printf "  mov rax, %d\n" x; read_tokens rest
  | (Symbol '+') :: rest ->
    let next_token = List.hd rest in (
      match next_token with
      | NUM y -> print_add y; read_tokens @@ List.tl rest
      | _ -> print_endline "無効なトークンです。"
    )
  | (Symbol '-') :: rest ->
    let next_token = List.hd rest in (
      match next_token with
      | NUM y -> print_sub y; read_tokens @@ List.tl rest
      | _ -> print_endline "無効なトークンです。"
    )
  | (ERROR x) :: rest -> Printf.printf "ERROR: %s" x
  | EOF :: rest -> print_endline "  ret"
  | _ -> ()


let () =
  print_first ();
  print_endline "_main:";
  read_tokens @@ tokenize p true
