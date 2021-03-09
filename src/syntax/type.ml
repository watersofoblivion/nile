open Format

type t =
  | Unit of Loc.t
  | Bool of Loc.t
  | Int of Loc.t
  | Float of Loc.t
  | String of Loc.t
  | Blob of Loc.t
  | Timestamp of Loc.t
  | Duration of Loc.t
  | Fun of Loc.t * t list * t
  | Tuple of Loc.t * int * t list
  | Record of Loc.t * field list
  | Variant of Loc.t * constr list
  | Package of Loc.t * t Sym.map * t Sym.map
and field = Loc.t * Sym.sym * t
and constr = Loc.t * Sym.sym * t option

let unit loc = Unit loc
let bool loc = Bool loc
let int loc = Int loc
let float loc = Float loc
let string loc = String loc
let blob loc = Blob loc
let timestamp loc = Timestamp loc
let duration loc = Duration loc
let func loc args ret = Fun (loc, args, ret)
let tuple loc tys = Tuple (loc, List.length tys, tys)
let record loc fields = Record (loc, fields)
let variant loc constrs = Variant (loc, constrs)
let pkg loc tys fns = Package (loc, tys, fns)

let field loc id ty = (loc, id, ty)
let constr loc id ty = (loc, id, ty)

let loc = function
  | Unit loc
  | Bool loc
  | Int loc
  | Float loc
  | String loc
  | Blob loc
  | Timestamp loc
  | Duration loc
  | Func (loc, _, _)
  | Tuple (loc, _, _)
  | Record (loc, _)
  | Variant (loc, _)
  | Package (loc, _, _) -> loc

let rec equal x y = match x, y with
  | Unit _, Unit _
  | Bool _, Bool _
  | Int _, Int _
  | Float _, Float _
  | String _, String _
  | Blob _, Blob _
  | Timestamp _, Timestamp _
  | Duration _, Duration _ -> true
  | Fun (_, args, ret), Fun (_, args', ret') ->
    List.for_all2 equal args args'
      && equal ret ret'
  | Tuple (_, len, tys), Tuple (_, len', tys') ->
    len = len' && List.for_all2 equal tys tys'
  | Record (_, fields), Record (_, fields') ->
    let match_field (id, ty) =
      try
        let (_, ty') = List.assoc id fields' in
        equal ty ty'
      with Not_found -> false
    in
    List.for_all match_field fields
  | Variant (_, constrs), Variant (_, constrs') ->
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
  | Patt.Unit _, Unit _
  | Patt.Int _, Int _
  | Patt.Bool _, Bool _
  | Patt.Float _, Float _
  | Patt.String _, String _
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
