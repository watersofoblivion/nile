open Format

(* Patterns *)

type atom =
  | Ground
  | Nil
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Var of Sym.sym

type expr =
  | Tuple of int * atom list
  | Cons of atom * atom
  | Constr of Sym.sym * atom option

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
let cons hd tl = Cons (hd, tl)
let constr id patt = Constr (id, patt)
