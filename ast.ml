module RecordType = Map.Make(String)
module Store = Map.Make(String)
(* module Modules = Map.Make(String) *)


type vtype =
  | TUnit
  | TInt
  | TBool
  | TChar
  | TPair of vtype * vtype
  | TRecord of vtype RecordType.t
  | TFunction of vtype * vtype
  | TSum of vtype * vtype

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
and sum =
  | Left of value
  | Right of value
and expr = 
  | Let of string * expr * expr
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
  | Match of expr * expr * expr
  | MakeLeft of expr
  | MakeRight of expr

type def =
  | DVal of string * expr
  | DType of string * vtype

type program = def list * expr