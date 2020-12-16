open Format

type t =
  | Int of Loc.t
  | Bool of Loc.t
  | Fun of Loc.t * t * t

let int loc = Int loc
let bool loc = Bool loc
let func loc a b = Fun (loc, a, b)

let rec pp ty fmt = match ty with
  | Int _ -> fprintf fmt "Int"
  | Bool _ -> fprintf fmt "Bool"
  | Fun (_, (Fun _ as a), b) -> fprintf fmt "(%t) -> %t" (pp a) (pp b)
  | Fun (_, a, b) -> fprintf fmt "%t -> %t" (pp a) (pp b)

let rec equal x y = match x, y with
  | Int _, Int _ | Bool _, Bool _ -> true
  | Fun (_, a, b), Fun (_, a', b') -> equal a a' && equal b b'
  | _ -> false

let loc = function
  | Bool loc
  | Int loc
  | Fun (loc, _, _) -> loc
