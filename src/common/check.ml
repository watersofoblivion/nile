module IdMap = Map.Make (struct
  type t = Sym.sym
  let compare = compare
end)

type env = Type.t IdMap.t

let env = IdMap.empty
let bind = IdMap.add
let lookup = IdMap.find

exception DeclarationMismatch of Sym.sym * Type.t * Type.t
exception ResultMismatch of Type.t * Type.t
exception UnboundIdentifier of Sym.sym
exception CannotApply of Type.t
exception TooManyArgs of Type.t * int
exception InvalidArgs of Type.t * Type.t
exception InvalidCondition of Type.t
exception ConditionalBranchMismatch of Type.t * Type.t
exception AnnotationRequired of Sym.sym

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
