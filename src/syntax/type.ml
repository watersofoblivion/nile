open Common

type t =
  | Fun of { loc: Loc.t;  params: t list; ret: t }
  | Tuple of { loc: Loc.t; arity: int; types: t list }
  | Record of { loc: Loc.t; fields: field list }
  | Variant of { loc: Loc.t; constrs: t list }
  | Package of { loc: Loc.t; types: t Sym.map; funs: t Sym.map }
  | Constr of { loc: Loc.t; names: Sym.sym list; params: t list }
and field = Field of { loc: Loc.t; name: Sym.sym; ty: t }

let func loc params ret = Fun { loc; params; ret }
let tuple loc types = Tuple { loc; arity = List.length types; types }
let record loc fields = Record { loc; fields }
let variant loc constrs = Variant { loc; constrs }
let pkg loc types funs = Package { loc; types; funs }
let constr loc names params = Constr { loc; names; params }

let field loc name ty = Field { loc; name; ty }

let loc = function
  | Fun ty -> ty.loc
  | Tuple ty -> ty.loc
  | Record ty -> ty.loc
  | Variant ty -> ty.loc
  | Package ty -> ty.loc
  | Constr ty -> ty.loc

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
