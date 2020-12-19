open Ast
open Check

exception IllegalExpression
exception IllegalValue
exception UndefinedVar
exception IllegalBinop
exception IllegalUnop
exception NotExhaustive

let update_store store store' = 
  Store.fold (fun k v acc -> Store.add k v acc) store' store

let rec same_pattern p v = 
  match p,v with
  | PUnit, Unit -> true
  | PWild, _ -> true
  | PBool b1, Bool b2 -> if b1 = b2 then true else false
  | PInt i1, Int i2 -> if i1 = i2 then true else false 
  | PChar c1, Char c2 -> if c1 = c2 then true else false
  | PVar v, _ -> true
  | PPair (a1, a2), Pair (b1, b2) -> same_pattern a1 b1 && same_pattern a2 b2
  | PRecord lst, Record r -> true
  | _ -> false

let rec bind_pattern (p : pat) (v : value) : value Store.t option =
  if not (same_pattern p v) then None else 
    match p,v with
    | PUnit, Unit -> Some (Store.empty)
    | PWild, _ -> Some (Store.empty)
    | PBool b1, Bool b2 -> Some (Store.empty)
    | PInt i1, Int i2 -> Some (Store.empty)
    | PChar c1, Char c2 ->  Some (Store.empty)
    | PVar b, _ -> Some (Store.add b v Store.empty)
    | PPair (a1, a2), Pair (b1, b2) -> Some (multi_bind a1 a2 b1 b2)
    | PRecord lst, Record r ->
      List.fold_left (fun acc s ->
          Option.bind acc (fun store ->
              RecordType.find_opt s r
              |> Option.map (fun v -> Store.add s v store)))
        (Some Store.empty) lst
    | _ -> None
and 
  multi_bind a1 a2 b1 b2 =
  let x = (bind_pattern a1 b1) in
  let y = (bind_pattern a2 b2) in
  begin match x with
    | Some s -> 
      begin 
        match y with 
        | Some s' -> update_store s s'
        | None -> s
      end
    | None -> 
      begin
        match y with 
        | Some s' -> s'
        | None -> Store.empty
      end
  end 


let eval_unop unop v1 = 
  match unop, v1 with
  | Not, Bool b -> Bool(not b)
  | _ -> raise IllegalUnop

let eval_binop bop v1 v2 = 
  match bop, v1, v2 with 
  | Add, Int n1, Int n2 -> Int (n1 + n2)
  | Sub, Int n1 , Int n2 -> Int (n1 - n2)
  | Mul, Int n1, Int n2 -> Int (n1 * n2)
  | Div, Int n1, Int n2 -> Int (n1 / n2)
  | Mod, Int n1, Int n2 -> Int (n1 mod n2)
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
  | Let (p, e1, e2) -> let v = eval_expr store e1 in
    let store' = 
      begin
        match bind_pattern p v with 
        | Some s -> s
        | None -> failwith "no bindings"
      end in
    eval_expr (update_store store store') e2
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
  | MakeLeft (_, e) -> Sum (Left (eval_expr store e))
  | MakeRight (_, e) -> Sum (Right (eval_expr store e))
  | Case (e1, e2, e3) -> begin
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
  | Import s -> let p = s
                        |> open_in
                        |> Lexing.from_channel 
                        |> Parser.program Lexer.token in
    typecheck_program p |> ignore; Record (p |> fst |> eval_defs)
  | Value v -> (match v with
      | Lazy (e, s) -> eval_expr s e
      | _ -> v)
  | Match (e, lst) -> eval_match store e lst
  | BinOp (bop, e1, e2) ->
    eval_binop bop (eval_expr store e1) (eval_expr store e2)
  | UnOp (uop, e) -> eval_unop uop (eval_expr store e)
  | Var v -> (match Store.find_opt v store with
      | None -> raise UndefinedVar
      | Some value -> Value value |> eval_expr store)
  | Fix e -> match eval_expr store e with
    | Function (v, s, e') ->
      let rec store' = Store.add v (Lazy (Fix e, store)) s in
      eval_expr store' e'
    | _ -> raise IllegalValue

and eval_match store e lst = 
  let v = eval_expr store e in 
  match lst with 
  | (p, e1)::t -> if same_pattern p v then
      let store' =
        begin 
          match bind_pattern p v with 
          | Some s -> s
          | None -> Store.empty
        end in 
      eval_expr (update_store store store') e1 
    else eval_match store e t
  | _ -> raise NotExhaustive

and eval_defs defs = List.fold_left (fun acc d -> match d with
    | DVal (l, e) -> let store' = begin
        match bind_pattern l (eval_expr acc e) with 
        | Some s -> s
        | None -> raise NotExhaustive
      end in
      update_store acc store'
    | DType _ -> acc) Store.empty defs

let eval_program (defs, e) =
  let store = eval_defs defs in eval_expr store e
