open Pprint
open Eval
open Check

let seq a b x = x |> a |> b

let () =
  Sys.argv.(2)
  |> open_in
  |> Lexing.from_channel 
  |> Parser.program Lexer.token
  |> (match Sys.argv.(1) with
      | "check" -> seq typecheck_program string_of_type
      | "run" -> seq eval_program string_of_value
      | _ -> fun _ -> "Invalid subcommand")
  |> print_endline