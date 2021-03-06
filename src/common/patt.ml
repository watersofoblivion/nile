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
and field =
  | Bare of Sym.sym
  | Named of Sym.sym * t

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
let record fields elipsis = Record (fields, elipsis)
let cons hd tl = Cons (hd, tl)
let or patts = Or patts

let bare name = Bare name
let named name patt = Named (name, patt)

(* Pretty Printing *)

let rec pp names patt fmt = match patt with
  | Ground -> pp_ground fmt
  | Nil -> pp_nil fmt
  | Unit -> pp_unit fmt
  | Bool b -> pp_bool b fmt
  | Int i -> pp_int i fmt
  | Float f -> pp_float f fmt
  | String s -> pp_string s fmt
  | Var sym -> pp_var names sym fmt
  | Tuple (_, patts) -> pp_tuple names patts fmt
  | Record (fields, elipsis) -> pp_record names fields elipsis fmt
  | Cons (hd, tl) -> pp_cons names hd tl fmt
  | Or patts -> pp_or names patts fmt

and pp_ground fmt =
  fprintf fmt "_"

and pp_nil fmt =
  fprintf fmt "nil"

and pp_unit fmt =
  fprintf fmt "()"

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_float f fmt =
  fprintf fmt "%f" f

and pp_string s fmt =
  fprintf fmt "%S" s

and pp_var names sym fmt =
  let id = Sym.name_of sym names in
  fprintf fmt "%s" id

and pp_tuple names patts fmt =

and pp_record names fields elipsis fmt =

and pp_cons names hd tl fmt =

and pp_or names patts fmt =

(* Type Checking *)

let rec irrefutable = function
  | Var _ | Ground | Nil -> true
  | Tuple (_, patts) -> List.for_all irrefutable patts
  | Cons (hd, tl) -> irrefutable hd && irrefutable tl
  | _ -> false
