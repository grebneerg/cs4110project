open Ast
open Printf

let rec string_of_type = function
  | TInt -> "int"
  | TUnit -> "unit"
  | TBool -> "bool"
  | TChar -> "char"
  | TPair (t1, t2) ->  sprintf "%s * %s" (string_of_type t1) (string_of_type t2)
  | TFunction (t1, t2) -> sprintf "%s -> %s" (string_of_type t1) (string_of_type t2)
  | TSum (t1, t2) -> sprintf "%s + %s" (string_of_type t1) (string_of_type t2)
  | TRecord r ->
    RecordType.fold
      (fun l t acc -> (sprintf "%s: %s" l (string_of_type t)) :: acc) r []
    |> String.concat ", "
    |> sprintf "{%s}"


let string_of_binop = function
  | Eq -> "="
  | Neq -> "!="
  | Gt -> ">"
  | Geq -> ">="
  | Lt -> "<"
  | Leq -> "<="
  | And -> "&&"
  | Or -> "||"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"

let string_of_unop = function
  | Not -> "not"

let rec string_of_pattern = function
  | PUnit -> "()"
  | PWild -> "_"
  | PInt n -> string_of_int n
  | PBool b -> string_of_bool b
  | PChar c -> String.make 1 c
  | PPair (v1, v2) ->
    sprintf "(%s, %s)" (string_of_pattern v1) (string_of_pattern v2)
and 
  string_of_value = function
  | Unit -> "()"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Char c -> String.make 1 c
  | Pair (v1, v2) ->
    sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | Record _ -> "some record"
  | Function (s, _, e) -> sprintf "func %s -> %s" s (string_of_expr e)
and string_of_expr = function
  | Let (p, e1, e2) ->
    sprintf "Let %s = %s in %s" (string_of_pattern p) (string_of_expr e1) (string_of_expr e2)
  | MakePair (e1, e2) ->
    sprintf "(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | MakeRec l -> List.map (fun (s, e) -> s ^ ": " ^ (string_of_expr e)) l
                 |> String.concat ", "
                 |> sprintf "(%s)"
  | Application (e1, e2) ->
    sprintf "%s %s" (string_of_expr e1) (string_of_expr e2)
  | If (e1, e2, e3) ->
    sprintf "if %s then %s else %s"
      (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Var s -> s
  | Value s -> string_of_value s
  | BinOp (b, e1, e2) ->
    sprintf "%s %s %s" (string_of_expr e1)
      (string_of_binop b) (string_of_expr e2)
  | UnOp (u, e) -> sprintf "%s %s" (string_of_unop u) (string_of_expr e)
  | MakeFunction (s, _, e) -> sprintf "func %s -> %s" s (string_of_expr e)
  | Fst e -> sprintf "fst %s" (string_of_expr e)
  | Snd e -> sprintf "snd %s" (string_of_expr e)

let string_of_program p = snd p |> string_of_expr