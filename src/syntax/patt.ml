open Common

(* Patterns *)

type t =
  | Unit of { loc: Loc.t }
  | Bool of { loc: Loc.t; lexeme: string }
  | Int of { loc: Loc.t; lexeme: string; radix: int }
  | Float of { loc: Loc.t; lexeme: string; hex: bool }
  | Rune of { loc: Loc.t; lexeme: string }
  | String of { loc: Loc.t; lexeme: string }
  | Byte of { loc: Loc.t; lexeme: string }
  | Blob of { loc: Loc.t; lexeme: string }
  | Timestamp of { loc: Loc.t; lexeme: string }
  | Duration of { loc: Loc.t; lexeme: string }
  | Ground of { loc: Loc.t }
  | Var of { loc: Loc.t; id: Sym.sym }
  | Tuple of { loc: Loc.t; arity: int; patts: t list }
  | Record of { loc: Loc.t; fields: field list; elipsis: bool }
  | Constr of { loc: Loc.t; name: Sym.sym list; args: t list }
  | Or of { loc: Loc.t; patts: t list }
and field =
  | Bare of { loc: Loc.t; name: Sym.sym }
  | Named of { loc: Loc.t; name: Sym.sym; patt: t }

(* Constructors *)

let unit loc = Unit { loc }
let bool loc lexeme = Bool { loc; lexeme }
let int loc lexeme radix = Int { loc; lexeme; radix }
let float loc lexeme hex = Float { loc; lexeme; hex }
let rune loc lexeme = Rune { loc; lexeme }
let string loc lexeme = String { loc; lexeme }
let byte loc lexeme = Byte { loc; lexeme }
let blob loc lexeme = Blob { loc; lexeme }
let timestamp loc lexeme = Timestamp { loc; lexeme }
let duration loc lexeme = Duration { loc; lexeme }
let ground loc = Ground { loc }
let var loc id = Var { loc; id }
let tuple loc patts = Tuple { loc; arity = List.length patts; patts }
let record loc fields elipsis = Record { loc; fields; elipsis }
let constr loc name args = Constr { loc; name; args }
let orr loc patts = Or { loc; patts }

let bare loc name = Bare { loc; name }
let named loc name patt = Named { loc; name; patt }

let loc = function
  | Unit patt -> patt.loc
  | Bool patt -> patt.loc
  | Int patt -> patt.loc
  | Float patt -> patt.loc
  | Rune patt -> patt.loc
  | String patt -> patt.loc
  | Byte patt -> patt.loc
  | Blob patt -> patt.loc
  | Timestamp patt -> patt.loc
  | Duration patt -> patt.loc
  | Ground patt -> patt.loc
  | Var patt -> patt.loc
  | Tuple patt -> patt.loc
  | Record patt -> patt.loc
  | Constr patt -> patt.loc
  | Or patt -> patt.loc

(* Type Checking *)

let rec irrefutable = function
  | Var _ | Ground _ -> true
  | Tuple patt -> List.for_all irrefutable patt.patts
  | _ -> false
