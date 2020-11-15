open Pprint
open Eval

let () =
  Sys.argv.(1)
  |> open_in
  |> Lexing.from_channel 
  |> Parser.program Lexer.token
  |> eval_program
  |> string_of_value
  |> print_endline