open Pprint
open Eval
open Check

let () =
  Sys.argv.(1)
  |> open_in
  |> Lexing.from_channel 
  |> Parser.program Lexer.token
  |> typecheck_program
  |> string_of_type
  |> print_endline