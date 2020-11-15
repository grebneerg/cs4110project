open Pprint

let () =
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let p = Parser.program Lexer.token lexbuf in
  string_of_program p |> print_endline