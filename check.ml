open Ast

open Pprint

exception NotExhaustive

let update_store store store' = 
  Store.fold (fun k v acc -> Store.add k v acc) store store'

let rec same_type_pat p t = 
  match p,t with
  | PUnit, TUnit -> true
  | PWild, _ -> true
  | PBool b1, TBool -> true
  | PInt i1, TInt -> true
  | PChar c1, TChar -> true
  | PVar v, _ -> true
  | PPair (a1, a2), TPair (b1, b2) -> same_type_pat a1 b1 && same_type_pat a2 b2
  | PRecord lst, TRecord r -> true
  | _ -> false

let rec bind_type_pat (p : pat) (t : vtype) : vtype Store.t option =
  if not (same_type_pat p t) then None else 
    match p,t with
    | PUnit, TUnit -> Some (Store.empty)
    | PWild, _ -> Some (Store.empty)
    | PBool b1, TBool  -> Some (Store.empty)
    | PInt i1, TInt -> Some (Store.empty)
    | PChar c1, TChar ->  Some (Store.empty)
    | PVar b, _ -> Some (Store.add b t Store.empty)
    | PPair (a1, a2), TPair (b1, b2) -> Some (multi_bind a1 a2 b1 b2)
    | PRecord lst, TRecord r ->
      List.fold_left (fun acc s ->
          Option.bind acc (fun store ->
              RecordType.find_opt s r
              |> Option.map (fun v -> Store.add s v store)))
        (Some Store.empty) lst
    | _ -> None
and 
  multi_bind a1 a2 b1 b2 =
  let x = (bind_type_pat a1 b1) in
  let y = (bind_type_pat a2 b2) in
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

let rec dealias aliases = function
  | TAlias s -> (match Store.find_opt s aliases with
      | Some t -> t
      | None -> failwith "Undefined alias")
  | TSum (l, r) -> TSum (dealias aliases l, dealias aliases r)
  | TPair (a, b) -> TPair (dealias aliases a, dealias aliases b)
  | TFunction (a, b) -> TFunction (dealias aliases a, dealias aliases b)
  | TRecord r -> TRecord(RecordType.map (dealias aliases) r)
  | t -> t

let rec realias aliases t = match Store.fold (fun k v acc -> if v = t then Some k else None) aliases None with
  | Some k -> TAlias k
  | None -> match t with
    | TSum (l, r) -> TSum (realias aliases l, realias aliases r)
    | TPair (a, b) -> TPair (realias aliases a, realias aliases b)
    | TFunction (a, b) -> TFunction (realias aliases a, realias aliases b)
    | TRecord r -> TRecord(RecordType.map (realias aliases) r)
    | t -> t

let max_re aliases t =
  let rec inner t = match realias aliases t with
    | t' when t' = t -> t
    | t' -> inner t' in
  inner t

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
  | Lazy (e, s) -> typecheck Store.empty Store.empty e
and typecheck aliases store e =
  let (=:=) t1 t2 = (max_re aliases t1) = (max_re aliases t2) in
  let typecheck store e = typecheck aliases store e in
  match e with
  | Let (p, e1, e2) -> let t = typecheck store e1 in
    let store' = 
      begin
        match bind_type_pat p t with 
        | Some s -> s
        | None -> failwith "no bindings"
      end in
    typecheck (update_store store store') e2
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
    (* let t = dealias aliases t in  *)
    let store' = Store.add s t store in TFunction (t, typecheck store' e)
  | MakeLeft (t, e) -> (match dealias aliases t with
      | TSum (t1, t2) -> if t1 =:= typecheck store e then TSum (t1, t2) else failwith "incorrect left type")
  | MakeRight (t, e) -> (match dealias aliases t with
      | TSum (t1, t2) -> if t2 =:= typecheck store e then TSum (t1, t2) else failwith "incorrect right type")
  | Case (e1, e2, e3) ->
    (match typecheck store e1 |> dealias aliases, typecheck store e2 |> dealias aliases, typecheck store e3 |> dealias aliases with
     | TSum(ta, tb), TFunction (t2, t3), TFunction (t4, t5)
       when ta =:= t2 && tb =:= t4 && t3 =:= t5 -> t5
     | _ -> failwith "Bad cases for sum types")
  | Application (e1, e2) -> (match typecheck store e1, typecheck store e2 with
      | TFunction (t1, t2), t3 when t1 =:= t3 -> t2
      | _ -> failwith "Bad application")
  | If (e1, e2, e3) -> begin 
      match typecheck store e1, typecheck store e2, typecheck store e3 with 
      | TBool, t2, t3 when t2 =:= t3 -> t2
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
  | Import s -> let p = s
                        |> open_in
                        |> Lexing.from_channel 
                        |> Parser.program Lexer.token in
    TRecord (p |> fst |> def_types |> snd)
  | UnOp (uop, e) -> let t1 = typecheck store e in
    begin 
      match uop with 
      | Not -> if t1 = TBool then TBool else failwith "wrong unop type"
    end 
  | Match (e, lst) ->
    List.fold_left (fun acc (p, e') ->
        typecheck store e
        |> bind_type_pat p
        |> Option.map (fun s -> typecheck (update_store store s) e')
        |> (fun o ->
            Option.bind o (fun t ->
                if t = Option.value acc ~default:t then Some t
                else raise NotExhaustive)))
      None lst
    |> Option.get
  | Var v -> (match Store.find_opt v store with
      | Some t -> t
      | None -> failwith "No var in scope")
  | Fix e -> match typecheck store e with 
    | TFunction (TFunction (t1, t2), TFunction (t3, t4)) when t1 =:= t3 && t2 =:= t4 -> TFunction (t1, t4)
    | _ -> failwith "fixing non-function"

and def_types defs = List.fold_left (fun (a, v) d -> match d with
    | DVal (l, e) -> let store' = begin 
        match bind_type_pat l (typecheck a v e) with 
        | Some s -> s
        | None -> failwith "no valid binding"
      end in (a, update_store v store')
    | DType (l, t) -> (Store.add l t a, v)) (Store.empty, Store.empty) defs

let typecheck_program (defs, e) =
  let (aliases, store) = def_types defs in
  typecheck aliases store e