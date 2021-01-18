open Format

type t =
  | Unit
  | Bool
  | Int
  | Fun of t * t
  | Tuple of int * t list

let unit = Unit
let bool = Bool
let int = Int
let func a b = Fun (a, b)
let tuple tys = Tuple (List.length tys, tys)

let rec pp ty fmt = match ty with
  | Unit -> fprintf fmt "Unit"
  | Bool -> fprintf fmt "Bool"
  | Int -> fprintf fmt "Int"
  | Fun ((Fun _ as a), b) -> fprintf fmt "(%t) -> %t" (pp a) (pp b)
  | Fun (a, b) -> fprintf fmt "%t -> %t" (pp a) (pp b)
  | Tuple (_, tys) ->
    fprintf fmt "(@[<hv 2>";
    let pp_sep fmt _ = fprintf fmt ",@ " in
    let pp_ty fmt ty = pp ty fmt in
    pp_print_list ~pp_sep pp_ty fmt tys;
    fprintf fmt "@])"

let rec equal x y = match x, y with
  | Unit, Unit
  | Bool, Bool
  | Int, Int -> true
  | Fun (a, b), Fun (a', b') -> equal a a' && equal b b'
  | Tuple (len, tys), Tuple (len', tys') ->
    let fold_left b ty ty' = b && equal ty ty' in
    len = len' && List.fold_left2 fold_left true tys tys'
  | _ -> false
