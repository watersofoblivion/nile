open Format

type t =
  | Unit
  | Bool
  | Int
  | Float
  | String
  | Blob
  | Timestamp
  | Duration
  | Fun of t list * t
  | Tuple of int * t list
  | Record of field list
  | Variant of constr list
  | Package of t Sym.map * t Sym.map
and field = Sym.sym * t
and constr = Sym.sym * t option

let unit = Unit
let bool = Bool
let int = Int
let float = Float
let string = String
let blob = Blob
let timestamp = Timestamp
let duration = Duration
let func args ret = Fun (args, ret)
let tuple tys = Tuple (List.length tys, tys)
let record fields = Record fields
let variant constrs = Variant constrs
let pkg tys fns = Package (tys, fns)

let field id ty = (id, ty)
let constr id ty = (id, ty)

let rec equal x y = match x, y with
  | Unit, Unit
  | Bool, Bool
  | Int, Int
  | Float, Float
  | String, String
  | Blob, Blob
  | Timestamp, Timestamp
  | Duration, Duration -> true
  | Fun (args, ret), Fun (args', ret') ->
    List.for_all2 equal args args'
      && equal ret ret'
  | Tuple (len, tys), Tuple (len', tys') ->
    len = len' && List.for_all2 equal tys tys'
  | Record fields, Record fields' ->
    let match_field (id, ty) =
      try
        let (_, ty') = List.assoc id fields' in
        equal ty ty'
      with Not_found -> false
    in
    List.for_all match_field fields
  | Variant constrs, Variant constrs' ->
    let match_constr constr =
      try
        let (_, ty') = List.assoc id constrs' in
        match ty, ty' with
          | None, None -> true
          | Some ty, Some ty' -> equal ty ty'
          | _ -> false
      with Not_found -> false
    in
    List.for_all match_constr constrs
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
