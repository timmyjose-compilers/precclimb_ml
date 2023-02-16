type unary_op = Uplus | Uminus

let pp_unary_op = function Uplus -> "+" | Uminus -> "-"

type binary_op = Plus | Minus | Mult | Div | Mod

let pp_binary_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"

type expression =
  | ExpInt of int
  | ExpUnary of unary_op * expression
  | ExpBinary of expression * binary_op * expression

let rec pp_expr = function
  | ExpInt n -> "ExpInt[" ^ string_of_int n ^ "]"
  | ExpUnary (op, exp) ->
      "ExpUnary { op = " ^ pp_unary_op op ^ ", exp = " ^ pp_expr exp ^ " }"
  | ExpBinary (exp1, op, exp2) ->
      "ExpBinary { exp1 = " ^ pp_expr exp1 ^ ", op = " ^ pp_binary_op op
      ^ ", exp2 = " ^ pp_expr exp2 ^ " }"
