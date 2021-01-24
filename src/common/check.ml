module IdMap = Map.Make (struct
  type t = Sym.t
  let compare = compare
end)

type env = Type.t IdMap.t

let env = IdMap.empty
let bind = IdMap.add
let lookup = IdMap.find

exception DeclarationMismatch of Sym.t * Type.t * Type.t
exception ResultMismatch of Type.t * Type.t
exception UnboundIdentifier of Sym.t
exception CannotApply of Type.t
exception TooManyArgs of Type.t * int
exception InvalidArgs of Type.t * Type.t
exception InvalidCondition of Type.t
exception ConditionalBranchMismatch of Type.t * Type.t
exception AnnotationRequired of Sym.t

let declaration_mismatch id expected actual =
  DeclarationMismatch (id, expected, actual)
    |> raise

let result_mismatch expected actual =
  ResultMismatch (expected, actual)
    |> raise

let unbound_identifier id =
  UnboundIdentifier id
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

let annotation_required id =
  AnnotationRequired id
    |> raise
