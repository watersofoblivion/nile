open Format

type t =
  | Int
  | Bool
  | Fun of t * t

let int = Int
let bool = Bool
let func a b = Fun (a, b)

let rec pp ty fmt = match ty with
  | Int -> fprintf fmt "Int"
  | Bool -> fprintf fmt "Bool"
  | Fun (Fun _ as a, b) -> fprintf fmt "(%t) -> %t" (pp a) (pp b)
  | Fun (a, b) -> fprintf fmt "%t -> %t" (pp a) (pp b)

let rec equal x y = match x, y with
  | Int, Int | Bool, Bool -> true
  | Fun (a, b), Fun (a', b') -> equal a a' && equal b b'
  | _ -> false
