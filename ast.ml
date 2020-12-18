module RecordType = Map.Make(String)
module Store = Map.Make(String)
(* module Modules = Map.Make(String) *)

type pat = 
  | PUnit
  | PWild
  | PInt of int
  | PBool of bool
  | PChar of char
  | PVar of string
  | PPair of pat * pat
  | PRecord of pat RecordType.t

type vtype =
  | TUnit
  | TInt
  | TBool
  | TChar
  | TPair of vtype * vtype
  | TRecord of vtype RecordType.t
  | TFunction of vtype * vtype
  | TSum of vtype * vtype
  | TAlias of string
  (* | TRecursive of  *)

type binop =
  | Eq 
  | Neq
  | Gt
  | Geq
  | Lt
  | Leq 
  | And 
  | Or
  | Add
  | Sub
  | Mul

type unop =
  | Not

type value =
  | Unit
  | Int of int
  | Bool of bool
  | Char of char
  | Pair of value * value
  | Record of value RecordType.t
  | Function of string * value Store.t * expr
  | Sum of sum
  | Lazy of expr * value Store.t
and sum =
  | Left of value
  | Right of value
and expr = 
  | Let of pat * expr * expr
  | MakePair of expr * expr
  | Fst of expr
  | Snd of expr
  | MakeRec of (string * expr) list
  | RecAccess of expr * string
  | MakeFunction of string * vtype * expr
  | Application of expr * expr
  | If of expr * expr * expr
  | Var of string
  (* | ModUse of expr * string *)
  | Value of value
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Case of expr * expr * expr
  | MakeLeft of vtype * vtype * expr
  | MakeRight of vtype * vtype * expr
  | Import of string
  | Match of expr * (pat * expr) list
  | Fix of expr

type def =
  | DVal of pat * expr
  | DType of pat * vtype

type program = def list * expr