(** {1 Type Checking} *)

(** {2 Environment} *)

type env
(** A type environment *)

val env : env
(** [env] constructs an empty type environment. *)

val bind : Sym.sym -> Type.t -> env -> env
(** [bind sym ty env] constructs copy of [env] extended by binding the type [ty]
    to the symbol [sym].  Any type previously bound to [sym] is masked.  The
    original environment is left unaltered. *)

val lookup : Sym.sym -> env -> Type.t
(** [lookup sym env] finds the type bound to the symbol [sym] in the environment
    [env], or raises {!Not_found} if the symbol is unbound. *)

(** {2 Errors} *)

exception DeclarationMismatch of Sym.sym * Type.t * Type.t
(** Raised when the declared type of a let-bound value does not match its actual
    type.  Contains the identifier, the expected type, and the actual type. *)

exception ResultMismatch of Type.t * Type.t
(** Raised when the declared result type of a let-bound function does not match
    the type of the function body.  Contains the expected and actual types. *)

exception UnboundIdentifier of Sym.sym
(** Raised when an identifier has not been bound and therefore has no type.
    Contains the identifier that was unbound. *)

exception CannotApply of Type.t
(** Raised when a value that is not a function is applied to an argument.
    Contains the type of the value that was applied. *)

exception TooManyArgs of Type.t * int
(** Raised when a function is applied to too many arguments.  Contains the type
    of the function and the number of arguments it was applied to. *)

exception InvalidArgs of Type.t * Type.t
(** Raised when a function is applied to an argument of invalid type.  Contains
    the expected and the actual argument types. *)

exception InvalidCondition of Type.t
(** Raised when the condition clause of a conditional statement is not a boolean
    typed value.  Contains the actual type of the condition clause. *)

exception ConditionalBranchMismatch of Type.t * Type.t
(** Raised when the true and false branches of a conditional statement do not
    have the same type.  Contains the types of the true and false branches. *)

exception AnnotationRequired of Sym.sym
(** Raised when a required annotation is not present.  Contains the
    identifier that requires an annotation. *)

val declaration_mismatch : Sym.sym -> Type.t -> Type.t -> 'a
(** [declaration_mismatch sym expected actual] raises a {!DeclarationMismatch}
    exception. *)

val result_mismatch : Type.t -> Type.t -> 'a
(** [result_mismatch expected actual] raises a {!ResultMismatch} exception. *)

val unbound_identifier : Sym.sym -> 'a
(** [unbound_identifier sym] raises an {!UnboundIdentifier} exception. *)

val cannot_apply : Type.t -> 'a
(** [cannot_apply ty] raises a {!CannotApply} exception. *)

val invalid_args : Type.t -> Type.t -> 'a
(** [invalid_args expected actual] raises an {!InvalidArgs} exception. *)

val invalid_condition : Type.t -> 'a
(** [invalid_condition ty] raises an {!InvalidCondition} exception. *)

val conditional_branch_mismatch : Type.t -> Type.t -> 'a
(** [conditional_branch_mismatch t f] raises an {!ConditionalBranchMismatch}
    exception. *)

val annotation_required : Sym.sym -> 'a
(** [annotation_required sym] raises an {!AnnotationRequired} exception. *)
