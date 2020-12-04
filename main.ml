open Ast

let () =
  print_endline "Enter name of file to test. (e.g. test0x.guac, where x is in [0,9])";
  print_string ">> ";
  let file = open_in (read_line ()) in
  let lexbuf = Lexing.from_channel file in
  let e =
    try Parser.prog Lexer.read lexbuf
    with _ ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at %d:%d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1 in

  print_string (string_of_expr e); print_newline ();
  let e' = Eval.eval e in
  print_string (string_of_expr e'); print_newline ()
