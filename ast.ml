module RecordType = Map.Make(String)
module Store = Map.Make(String)


type vtype =
  | TUnit
  | TInt
  | TBool
  | TChar
  | TPair of vtype * vtype
  | TRecord of vtype RecordType.t
  | TFunction of vtype * vtype

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
and expr = 
  | Let of string * expr * expr
  | MakePair of expr * expr
  | MakeRec of (string * expr) list
  | MakeFunction of (string * expr)
  | Application of expr * expr
  | If of expr * expr * expr
  | Var of string
  | Value of value
  | BinOp of binop * expr * expr
  | UnOp of unop * expr

type def =
  | DVal of string * expr
  | DType of string * vtype

type program = def list * expr