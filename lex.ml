open Codegen

(* 文字列をcharのlistにする *)
let explode s = List.init (String.length s) (String.get s)

type tokens =
  | Num_t of int
  | Add_t
  | Sub_t
  | Mul_t
  | Div_t
  | LP
  | RP

let is_int c = let i = int_of_char c in
  if 48 <= i && i <= 57 then true else false

let int_of_chars lst =
  int_of_string @@ String.of_seq @@ List.to_seq lst


let rec extraction_int lst result = 
  match lst with
  | [] -> (int_of_chars result, [])
  | first :: rest when is_int first -> extraction_int rest (result @ [first])
  | _ -> (int_of_chars result, lst)


let rec generate_tokens chars =
  match chars with
  | [] -> []
  | ' ' :: rest -> generate_tokens rest
  | '(' :: rest -> LP :: generate_tokens rest
  | ')' :: rest -> RP :: generate_tokens rest
  | '+' :: rest -> Add_t :: generate_tokens rest
  | '-' :: rest -> Sub_t :: generate_tokens rest
  | '*' :: rest -> Mul_t :: generate_tokens rest
  | '/' :: rest -> Div_t :: generate_tokens rest
  | x :: rest when is_int x -> 
    let value, rest' = extraction_int chars [] in Num_t value :: generate_tokens rest'
  | _ -> []


let tokenize text = generate_tokens @@ explode text


let rec treenize token_list =
  match token_list with
  | LP :: rest -> 
    let lhs, rest' = treenize_to_rp rest 0 [] in
    read_next rest' lhs
  | Num_t x :: [] -> Num x
  | Num_t x :: rest ->
    read_next rest (Num x)
  | _ -> Error "無効なトークンです"

and treenize_to_rp token_list p_count result =
  match token_list with
  | first :: rest when first = RP -> 
    if p_count = 0 
      then (treenize result, rest) 
      else treenize_to_rp rest (p_count-1) (result @ [first])
  | first :: rest when first = LP -> 
    treenize_to_rp rest (p_count+1) (result @ [first])
  | first :: rest ->
    treenize_to_rp rest p_count (result @ [first])
  | _ -> (Error "カッコの終わりがたりません", [])
and read_next token_list lhs =
  match token_list with
  | [] -> lhs
  | Add_t :: rest -> (
    match rest with
    | Num_t x :: rest' ->
      read_next rest' (Add (lhs, Num x))
    | LP :: rest' ->
    let rhs, rest'' = treenize_to_rp rest' 0 [] in
      read_next rest'' (Add (lhs, rhs))
    | _ -> Error "無効なトークンです。"
  )
  | _ :: rest -> (
    match rest with
    | Num_t x :: rest' ->
      read_next rest' (Sub (lhs, Num x))
    | LP :: rest' ->
    let rhs, rest'' = treenize_to_rp rest' 0 [] in
      read_next rest'' (Sub (lhs, rhs))
    | _ -> Error "無効なトークンです。"
  )




