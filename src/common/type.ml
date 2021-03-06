open Format

type t =
  | Unit
  | Bool
  | Int
  | Float
  | String
  | Fun of t list * t
  | Tuple of int * t list
  | Package of t Sym.map * t Sym.map

let unit = Unit
let bool = Bool
let int = Int
let float = Float
let string = String
let func args ret = Fun (args, ret)
let tuple tys = Tuple (List.length tys, tys)
let pkg tys fns = Package (tys, fns)

let rec pp ty fmt = match ty with
  | Unit -> fprintf fmt "Unit"
  | Bool -> fprintf fmt "Bool"
  | Int -> fprintf fmt "Int"
  | Float -> fprintf fmt "Float"
  | String -> fprintf fmt "String"
  | Fun (args, ret) ->
    let pp_arg fmt = function
      | Fun _ as f -> fprintf fmt "(%t)" (pp f)
      | arg -> pp arg fmt
    in
    let pp_sep fmt _ = fprintf fmt " -> " in
    pp_print_list ~pp_sep pp_arg fmt args;
    fprintf fmt " -> %t" (pp ret)
  | Tuple (_, tys) ->
    fprintf fmt "(@[<hv 2>";
    let pp_sep fmt _ = fprintf fmt ",@ " in
    let pp_ty fmt ty = pp ty fmt in
    pp_print_list ~pp_sep pp_ty fmt tys;
    fprintf fmt "@])"

let rec equal x y = match x, y with
  | Unit, Unit
  | Bool, Bool
  | Int, Int
  | Float, Float
  | String, String -> true
  | Fun (args, ret), Fun (args', ret') ->
    List.for_all2 equal args args'
      && equal ret ret'
  | Tuple (len, tys), Tuple (len', tys') ->
    len = len' && List.for_all2 equal tys tys'
  | _ -> false

module IdMap = Map.Make (struct
  type t = Sym.sym
  let compare = compare
end)

type env = t Sym.map

let env = Sym.empty
let bind patt ty env = match patt with
  | Patt.Var sym -> Sym.bind sym ty env
  | _ -> env
let lookup = Sym.lookup

let of_pattern patt ty = match patt, ty with
  | Patt.Unit, Unit
  | Patt.Int _, Int
  | Patt.Bool _, Bool
  | Patt.Float _, Float
  | Patt.String _, String
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
