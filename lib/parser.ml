open Lexer
open Ast

exception ParserError of string

type parser = { lexer : lexer }

let init_parser l = { lexer = l }

let get_unary_op tok =
  match tok with
  | Tplus -> Uplus
  | Tminus -> Uminus
  | _ ->
      raise (ParserError ("invalid token for unary operator: " ^ pp_token tok))

let get_binary_op tok =
  match tok with
  | Tplus -> Plus
  | Tminus -> Minus
  | Tmult -> Mult
  | Tdiv -> Div
  | Tmod -> Mod
  | _ ->
      raise (ParserError ("invalid token for binary operator: " ^ pp_token tok))

let rec parse_expression p =
  let rec parse_primary p =
    let tok = lex p.lexer in
    match tok with
    | Tint n -> ExpInt n
    | Tlp ->
        let e = parse_expression p in
        let rp = peek p.lexer in
        if rp <> Trp then
          raise (ParserError "missing parenthesis while parsing expression")
        else
          let _ = lex p.lexer in
          e
    | Tplus | Tminus ->
        let prim = ref (parse_primary p) in
        let e = parse_expression_aux p prim max_precedence in
        ExpUnary (get_unary_op tok, e)
    | _ -> raise (ParserError ("invalid primary expression: " ^ pp_token tok))
  and parse_expression_aux p lhs min_prec =
    let lookahead = ref (peek p.lexer) in
    while is_binary_op !lookahead && precedence !lookahead >= min_prec do
      let op = ref !lookahead in
      let _ = lex p.lexer in
      let rhs = ref (parse_primary p) in
      lookahead := peek p.lexer;
      while
        (is_binary_op !lookahead
        && (precedence !lookahead > precedence !op))
           || is_right_assoc !lookahead
              && precedence !lookahead == precedence !op
      do
        rhs := parse_expression_aux p rhs (precedence !lookahead);
        lookahead := peek p.lexer
      done;
      lhs := ExpBinary (!lhs, get_binary_op !op, !rhs)
    done;
    !lhs
  in
  let lhs = ref (parse_primary p) in
  parse_expression_aux p lhs min_precdence

let parse = parse_expression
