module type M =
  sig
    type id

    type env
    val env : env
    val bind : id -> Type.t -> env -> env
    val lookup : id -> env -> Type.t

    exception DeclarationMismatch of id * Type.t * Type.t
    exception ResultMismatch of Type.t * Type.t
    exception UnboundIdentifier of id
    exception CannotApply of Type.t
    exception TooManyArgs of Type.t * int
    exception InvalidArgs of Type.t * Type.t
    exception InvalidCondition of Type.t
    exception ConditionalBranchMismatch of Type.t * Type.t
    exception AnnotationRequired of id

    val declaration_mismatch : id -> Type.t -> Type.t -> 'a
    val result_mismatch : Type.t -> Type.t -> 'a
    val unbound_identifier : id -> 'a
    val cannot_apply : Type.t -> 'a
    val invalid_args : Type.t -> Type.t -> 'a
    val invalid_condition : Type.t -> 'a
    val conditional_branch_mismatch : Type.t -> Type.t -> 'a
    val annotation_required : id -> 'a
  end

module Make = functor (Id: Map.OrderedType) ->
  struct
    type id = Id.t

    module IdMap = Map.Make (Id)

    type env = Type.t IdMap.t

    let env = IdMap.empty
    let bind = IdMap.add
    let lookup = IdMap.find

    exception DeclarationMismatch of id * Type.t * Type.t
    exception ResultMismatch of Type.t * Type.t
    exception UnboundIdentifier of id
    exception CannotApply of Type.t
    exception TooManyArgs of Type.t * int
    exception InvalidArgs of Type.t * Type.t
    exception InvalidCondition of Type.t
    exception ConditionalBranchMismatch of Type.t * Type.t
    exception AnnotationRequired of id

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
  end