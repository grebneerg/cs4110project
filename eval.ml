open Ast

exception IllegalExpression
exception IllegalValue
exception UndefinedVar
exception IllegalBinop
exception IllegalUnop

let eval_unop unop v1 = 
  match unop, v1 with
  | Not, Bool b -> Bool(not b)
  | _ -> raise IllegalUnop

let eval_binop bop v1 v2 = 
  match bop, v1, v2 with 
  | Add, Int n1, Int n2 -> Int (n1 + n2)
  | Sub, Int n1 , Int n2 -> Int (n1 - n2)
  | Mul, Int n1, Int n2 -> Int (n1 * n2)
  | Eq, Int n1, Int n2 -> Bool (n1 = n2)
  | Neq, Int n1, Int n2 -> Bool (n1 <> n2)
  | Lt, Int n1, Int n2 -> Bool (n1 < n2)
  | Leq, Int n1, Int n2 -> Bool (n1 <= n2)
  | Gt, Int n1, Int n2 -> Bool (n1 > n2)
  | Geq, Int n1, Int n2 -> Bool(n1 >= n2)
  | And, Bool b1, Bool b2 -> Bool (b1 && b2)
  | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
  | _ -> raise IllegalBinop


let rec eval_expr (store: value Store.t) = function
  | Let (s, e1, e2) -> let v = eval_expr store e1 in
    let store' = Store.add s v store in
    eval_expr store' e2
  | MakePair (e1, e2) -> Pair (eval_expr store e1, eval_expr store e2)
  | Fst e -> begin
      match eval_expr store e with
      | Pair (e1, e2) -> e1
      | _ -> raise IllegalExpression
    end
  | Snd e -> begin
      match eval_expr store e with
      | Pair (e1, e2) -> e2
      | _ -> raise IllegalExpression
    end
  | MakeRec lst ->
    List.fold_left
      (fun (map : Ast.value RecordType.t) (l, e) ->
         RecordType.add l (eval_expr store e) map)
      RecordType.empty lst
    |> fun m -> Record m
  | RecAccess (e, l) ->
    e
    |> eval_expr store
    |> (function
        | Record r -> RecordType.find_opt l r
        | _ -> None)
    |> (function
        | Some v -> v
        | None -> raise IllegalExpression)
  | MakeFunction (s, _, e) -> Function (s, store, e)
  | MakeLeft (_, _, e) -> Sum (Left (eval_expr store e))
  | MakeRight (_, _, e) -> Sum (Right (eval_expr store e))
  | Match (e1, e2, e3) -> begin
      match eval_expr store e1 with
      | Sum (Left v) -> Application (e2, Value v) |> eval_expr store
      | Sum (Right v)-> Application (e3, Value v) |> eval_expr store 
      | _ -> raise IllegalValue
    end
  | Application (e1, e2) -> begin
      match eval_expr store e1 with
      | Function (s, st, e) -> let p = eval_expr store e2 in
        let st' = Store.add s p st in eval_expr st' e
      | _ -> raise IllegalExpression
    end
  | If (e1, e2, e3) -> begin
      let b = eval_expr store e1 in match b with
      | Bool b -> if b then eval_expr store e2 else eval_expr store e3
      | _ -> raise IllegalValue
    end
  | Value v -> v
  | BinOp (bop, e1, e2) ->
    eval_binop bop (eval_expr store e1) (eval_expr store e2)
  | UnOp (uop, e) -> eval_unop uop (eval_expr store e)
  | Var v -> match Store.find_opt v store with
    | None -> raise UndefinedVar
    | Some value -> value

let rec eval_program (defs, e) =
  let store = List.fold_left (fun acc d -> match d with
      | DVal (l, e) -> Store.add l (eval_expr acc e) acc
      | DType _ -> acc) Store.empty defs in eval_expr store e
