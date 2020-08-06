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


let rec treenize token_list = read_expr token_list []

and read_expr lst lhs =
  match lst with
  | [] -> Num (-255)
  | first :: rest -> begin 
    match first with
    | Add_t -> Add (read_mul lhs [], read_mul rest [])
    | Sub_t -> Sub (read_mul lhs [], read_mul rest [])
    | t -> read_expr rest (lhs @ [t])
  end

and read_mul lst lhs =
  match lst with
  | [] -> read_expr lhs []
  | first :: rest -> begin 
    match first with
    | Mul_t -> Div (read_primary lhs, read_primary rest)
    | Div_t -> Div (read_primary lhs, read_primary rest)
    | t -> read_mul rest (lhs @ [t])
  end
and read_primary lst =
  match lst with
  | LP :: tl -> begin 
    match List.rev tl with
      | RP :: rest -> read_expr (List.rev rest) []
      | _ -> Num (-255)(* エラー *)
  end
  | [Num_t x] -> Num x
  | _ -> Num (-255) (* エラー *)

