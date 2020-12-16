open Format

type un =
  | Not of Loc.t

let un_not loc = Not loc

let pp_un op fmt = match op with
  | Not _ -> fprintf fmt "!"

let un_precedence = function
  | Not _ -> 2

let un_loc = function
  | Not loc -> loc

type bin =
  | Add of Loc.t
  | Sub of Loc.t
  | Mul of Loc.t
  | Div of Loc.t
  | Mod of Loc.t
  | And of Loc.t
  | Or of Loc.t
  | Eq of Loc.t
  | Neq of Loc.t
  | Lte of Loc.t
  | Lt of Loc.t
  | Gt of Loc.t
  | Gte of Loc.t

let bin_add loc = Add loc
let bin_sub loc = Sub loc
let bin_mul loc = Mul loc
let bin_div loc = Div loc
let bin_mod loc = Mod loc
let bin_and loc = And loc
let bin_or loc = Or loc
let bin_eq loc = Eq loc
let bin_neq loc = Neq loc
let bin_lte loc = Lte loc
let bin_lt loc = Lt loc
let bin_gt loc = Gt loc
let bin_gte loc = Gte loc

let pp_bin op fmt = match op with
  | Add _ -> fprintf fmt "+"
  | Sub _ -> fprintf fmt "-"
  | Mul _ -> fprintf fmt "*"
  | Div _ -> fprintf fmt "/"
  | Mod _ -> fprintf fmt "%%"
  | And _ -> fprintf fmt "&&"
  | Or _ -> fprintf fmt "||"
  | Eq _ -> fprintf fmt "=="
  | Neq _ -> fprintf fmt "!="
  | Lte _ -> fprintf fmt "<="
  | Lt _ -> fprintf fmt "<"
  | Gt _ -> fprintf fmt ">"
  | Gte _ -> fprintf fmt ">="

let bin_precedence = function
  | Mul _ | Div _ | Mod _ -> 3
  | Add _ | Sub _ -> 4
  | Lte _ | Lt _ | Gt _ | Gte _ -> 6
  | Eq _ | Neq _ -> 7
  | And _ -> 11
  | Or _ -> 12

let bin_loc = function
  | Add loc | Sub loc | Mul loc | Div loc | Mod loc
  | And loc | Or loc
  | Eq loc | Neq loc
  | Lte loc | Lt loc | Gt loc | Gte loc -> loc
