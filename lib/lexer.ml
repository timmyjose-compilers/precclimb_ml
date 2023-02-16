type token =
  | Tint of int
  | Tplus
  | Tminus
  | Tmult
  | Tdiv
  | Tmod
  | Tlp
  | Trp
  | Tend

let min_precdence = 0
let max_precedence = 100

let precedence = function
  | Tplus | Tminus -> 20
  | Tmult | Tdiv -> 30
  | Tmod -> 10
  | _ -> min_precdence

let is_right_assoc = function
  _ -> false

let is_binary_op = function
  | Tplus | Tminus | Tmult | Tdiv | Tmod -> true
  | _ -> false

let pp_token = function
  | Tint n -> string_of_int n
  | Tplus -> "+"
  | Tminus -> "-"
  | Tmult -> "*"
  | Tdiv -> "/"
  | Tmod -> "%"
  | Tlp -> "("
  | Trp -> ")"
  | Tend -> "<eof>"

exception LexerError of string

type lexer = { src : string; mutable curr_pos : int; size : int }

let init_lexer src = { src; curr_pos = 0; size = String.length src }
let forward l = l.curr_pos <- l.curr_pos + 1
let forward_n l n = l.curr_pos <- l.curr_pos + n
let backward l = l.curr_pos <- l.curr_pos - 1
let backward_n l n = l.curr_pos <- l.curr_pos - n

let extract_pred pred l =
  let rec check pred l pos =
    if pos < l.size && pred l.src.[pos] then check pred l (pos + 1) else pos
  in
  let start_pos = l.curr_pos in
  let end_pos = check pred l start_pos in
  l.curr_pos <- end_pos;
  (end_pos - start_pos, String.sub l.src start_pos (end_pos - start_pos))

let extract_int l =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let npos, intstr = extract_pred is_digit l in
  (npos, int_of_string intstr)

let rec peek l =
  let peek_char l c =
    match c with
    | ' ' | '\t' | '\n' ->
        forward l;
        peek l
    | '0' .. '9' ->
        let npos, i32 = extract_int l in
        backward_n l npos;
        Tint i32
    | '+' -> Tplus
    | '-' -> Tminus
    | '*' -> Tmult
    | '/' -> Tdiv
    | '%' -> Tmod
    | '(' -> Tlp
    | ')' -> Trp
    | _ -> raise (LexerError ("invalid character: " ^ String.make 1 c))
  in
  if l.curr_pos >= l.size then Tend else peek_char l l.src.[l.curr_pos]

let rec lex l =
  let lex_char l c =
    match c with
    | ' ' | '\t' | '\n' ->
        forward l;
        lex l
    | '0' .. '9' ->
        let _npos, i32 = extract_int l in
        Tint i32
    | '+' ->
        forward l;
        Tplus
    | '-' ->
        forward l;
        Tminus
    | '*' ->
        forward l;
        Tmult
    | '/' ->
        forward l;
        Tdiv
    | '%' ->
        forward l;
        Tmod
    | '(' ->
        forward l;
        Tlp
    | ')' ->
        forward l;
        Trp
    | _ -> raise (LexerError ("invalid character: " ^ String.make 1 c))
  in
  if l.curr_pos >= l.size then Tend else lex_char l l.src.[l.curr_pos]
