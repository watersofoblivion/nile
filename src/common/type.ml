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
  module IdMap = Map.Make (struct
    type t = Sym.sym
    let compare = compare
  end)

type env = t IdMap.t

let env = IdMap.empty
let bind patt ty env = match patt with
  | Patt.Var sym -> IdMap.add sym ty env
  | _ -> env
let lookup = IdMap.find

let of_pattern patt ty = match patt, ty with
  | Patt.Unit, Unit
  | Patt.Int _, Int
  | Patt.Bool _, Bool
  | Patt.Var _, _
  | Patt.Ground, _ -> true
  | _ -> false

exception DeclarationMismatch of Patt.t * t * t
exception ResultMismatch of t * t
exception UnboundIdentifier of Sym.sym
exception CannotApply of t
exception TooManyArgs of t * int
exception InvalidArgs of t * t
exception InvalidCondition of t
exception ConditionalBranchMismatch of t * t
exception AnnotationRequired of Patt.t

let declaration_mismatch sym expected actual =
  DeclarationMismatch (sym, expected, actual)
    |> raise

let result_mismatch expected actual =
  ResultMismatch (expected, actual)
    |> raise

let unbound_identifier sym =
  UnboundIdentifier sym
    |> raise

let cannot_apply ty =
  CannotApply ty
    |> raise
(*
let too_many_args ty num =
  TooManyArgs (ty, num)
    |> raise *)

let invalid_args expected actual =
  InvalidArgs (expected, actual)
    |> raise

let invalid_condition ty =
  InvalidCondition ty
    |> raise

let conditional_branch_mismatch t f =
  ConditionalBranchMismatch (t, f)
    |> raise

let annotation_required sym =
  AnnotationRequired sym
    |> raise
