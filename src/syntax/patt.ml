open Format

(* Patterns *)

type t =
  | Ground of Loc.t
  | Nil of Loc.t
  | Unit of Loc.t
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | Float of Loc.t * float
  | String of Loc.t * string
  | Var of Loc.t * Sym.sym
  | Tuple of Loc.t * int * t list
  | Record of Loc.t * field list * bool
  | Cons of Loc.t * t * t
  | Or of Loc.t * t list
and field =
  | Bare of Loc.t * Sym.sym
  | Named of Loc.t * Sym.sym * t

(* Constructors *)

let ground loc = Ground loc
let nil loc = Nil loc
let unit loc = Unit loc
let bool loc b = Bool (loc, b)
let int loc i = Int (loc, i)
let float loc f = Float (loc, f)
let string loc s = String (loc, s)
let var loc sym = Var (loc, sym)
let tuple loc patts = Tuple (loc, List.length patts, patts)
let record loc fields elipsis = Record (loc, fields, elipsis)
let cons loc hd tl = Cons (loc, hd, tl)
let orr loc patts = Or (loc, patts)

let bare loc name = Bare (loc, name)
let named loc name patt = Named (loc, name, patt)

let loc = function
  | Ground loc
  | Nil loc
  | Unit loc
  | Bool (loc, _)
  | Int (loc, _)
  | Float (loc, _)
  | String (loc, _)
  | Var (loc, _)
  | Tuple (loc, _, _)
  | Record (loc, _, _)
  | Cons (loc, _, _)
  | Or (loc, _) -> loc

(* Type Checking *)

let rec irrefutable = function
  | Var _ | Ground _ | Nil _ -> true
  | Tuple (_, _, patts) -> List.for_all irrefutable patts
  | Cons (_, hd, tl) -> irrefutable hd && irrefutable tl
  | _ -> false
