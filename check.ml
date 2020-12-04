open Ast

let rec type_of_value = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Unit -> TUnit
  | Char _ -> TChar
  | Record r ->
    TRecord (RecordType.fold
               (fun l v acc -> RecordType.add l (type_of_value v) acc)
               r RecordType.empty)
  | Pair (v1, v2) -> TPair ((type_of_value v1, type_of_value v2))
  | Function _ -> failwith "unimplemented"
  | Sum _ -> failwith "unimplemented"


let rec typecheck store = function
  | Let (s, e1, e2) -> let store' = Store.add s (typecheck store e1) store in
    typecheck store' e2
  | MakePair (e1, e2) -> TPair(typecheck store e1, typecheck store e2)
  | Fst e -> (match typecheck store e with
      | TPair (t1, _) -> t1
      | _ -> failwith "fst on non-pair")
  | Snd e -> (match typecheck store e with
      | TPair (_, t2) -> t2
      | _ -> failwith "snd on non-pair")
  | MakeRec lst ->
    TRecord (List.fold_left
               (fun acc (l, e) -> RecordType.add l (typecheck store e) acc)
               RecordType.empty lst)
  | RecAccess (e, l) -> (match typecheck store e with
      | TRecord tr -> (match RecordType.find_opt l tr with
          | Some t -> t
          | None -> failwith "invalid record field")
      | _ -> failwith "Not a record")
  | MakeFunction (s, t, e) ->
    let store' = Store.add s t store in TFunction (t, typecheck store' e)
  | MakeLeft (t1, t2, e) -> if t1 = typecheck store e then TSum (t1, t2) else failwith "incorrect left type"
  | MakeRight (t1, t2, e) -> if t2 = typecheck store e then TSum (t1, t2) else failwith "incorrect right type"
  | Match (e1, e2, e3) ->
    (match typecheck store e1, typecheck store e2, typecheck store e3 with
     | TSum(ta, tb), TFunction (t2, t3), TFunction (t4, t5)
       when ta = t2 && tb = t4 && t3 = t5 -> t5
     | _ -> failwith "Bad match types")
  | Application (e1, e2) -> (match typecheck store e1, typecheck store e2 with
      | TFunction (t1, t2), t3 when t1 = t3 -> t2
      | _ -> failwith "Bad application")
  | If (e1, e2, e3) -> begin 
      match typecheck store e1, typecheck store e2, typecheck store e3 with 
      | TBool, t2, t3 when t2 = t3 -> t2
      | _ -> failwith "bad if statement types"
    end
  | Value v -> type_of_value v
  | BinOp (bop, e1, e2) -> let t1 = typecheck store e1 in
    let t2 = typecheck store e2 in 
    begin
      match bop with
      | Add | Sub | Mul -> (match t1, t2 with  
          | TInt, TInt -> TInt 
          | _ -> failwith "wrong binop type, expected int")
      | Geq | Gt | Lt | Leq | Eq | Neq -> (match t1, t2 with
          | TInt, TInt -> TBool
          | _ ->  failwith "wrong binop type, expected int")
      | And | Or -> (match t1, t2 with
          | TBool, TBool -> TBool
          | _ -> failwith "Wrong binop type, expected bool")
    end
  | UnOp (uop, e) -> let t1 = typecheck store e in
    begin 
      match uop with 
      | Not -> if t1 = TBool then TBool else failwith "wrong unop type"
    end 
  | Var v -> match Store.find_opt v store with
    | Some t -> t
    | None -> failwith "No var in scope"

let typecheck_program (defs, e) =
  let store = List.fold_left (fun acc d -> match d with
      | DVal (l, e) -> Store.add l (typecheck acc e) acc
      | DType _ -> acc) Store.empty defs in typecheck store e