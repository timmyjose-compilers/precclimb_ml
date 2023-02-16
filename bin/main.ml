open Precclimb.Lexer
open Precclimb.Parser
open Precclimb.Ast

let prompt = ">> "

let rec main () =
  print_string prompt;
  let input = read_line () in
  let l = init_lexer input in
  let p = init_parser l in
  let ast = parse p in
  print_endline (pp_expr ast);
  main ()
;;

main ()
