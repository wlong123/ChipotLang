open Ast

(** Runs the command line client to launch tests. *)
let () =
  print_endline "Enter name of file to test. (e.g. test00.guac)";
  print_string ">> ";
  let file = open_in (read_line ()) in
  let lexbuf = Lexing.from_channel file in
  let e =
    try Parser.prog Lexer.read lexbuf
    with _ -> print_endline "Syntax error in file"; exit 1
  in
  print_endline ("Parsed input: " ^ (string_of_expr e));
  let e' = Eval.eval e in
  print_endline ("==> " ^ (string_of_expr e'))
