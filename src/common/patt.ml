open Format

(* Patterns *)

type t =
  | Ground
  | Bool of bool
  | Int of int
  | Var of Sym.sym

(* Constructors *)

let ground = Ground
let bool b = Bool b
let int i = Int i
let var sym = Var sym

(* Pretty Printing *)

let rec pp names patt fmt = match patt with
  | Ground -> pp_ground fmt
  | Bool b -> pp_bool b fmt
  | Int i -> pp_int i fmt
  | Var sym -> pp_var names sym fmt

and pp_ground fmt =
  fprintf fmt "_"

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_var names sym fmt =
  let id = Sym.name_of sym names in
  fprintf fmt "%s" id
