open Format

(* Patterns *)

type t =
  | Ground
  | Nil
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Var of Sym.sym
  | Tuple of int * t list
  | Record of field list * bool
  | Cons of t * t
  | Or of t list
and field = Sym.sym * t

(* Constructors *)

let ground = Ground
let nil = Nil
let unit = Unit
let bool b = Bool b
let int i = Int i
let float f = Float f
let string s = String s
let var sym = Var sym
let tuple patts = Tuple (List.length patts, patts)
let record fields = Record fields
let cons hd tl = Cons (hd, tl)
let orr patts = Or patts

let field name patt = (name, patt)
