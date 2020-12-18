open Format

type t =
  | Bool
  | Int
  | Fun of t * t

let bool = Bool
let int = Int
let func a b = Fun (a, b)

let rec pp ty fmt = match ty with
  | Bool -> fprintf fmt "Bool"
  | Int -> fprintf fmt "Int"
  | Fun (Fun _ as a, b) -> fprintf fmt "(%t) -> %t" (pp a) (pp b)
  | Fun (a, b) -> fprintf fmt "%t -> %t" (pp a) (pp b)
