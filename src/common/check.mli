(** {1 Type Checking} *)

(** {2 Environment} *)

type env
(** A type environment *)

val env : env
(** [env] constructs an empty type environment. *)

val bind : int -> Type.t -> env -> env
(** [bind id ty env] constructs copy of [env] extended by binding the type [ty]
    to the identifier [id].  Any type previously bound to [id] is masked.  The
    original environment is left unaltered. *)

val lookup : int -> env -> Type.t
(** [lookup id env] finds the type bound to the identifier [id] in the
    environment [env], or raises {!Not_found} if the identifier is unbound. *)

(** {2 Errors} *)

exception DeclarationMismatch of int * Type.t * Type.t
(** Raised when the declared type of a let-bound value does not match its actual
    type.  Contains the identifier, the expected type, and the actual type. *)

exception ResultMismatch of Type.t * Type.t
(** Raised when the declared result type of a let-bound function does not match
    the type of the function body.  Contains the expected and actual types. *)

exception UnboundIdentifier of int
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

exception AnnotationRequired of int
(** Raised when a required annotation is not present.  Contains the
    identifier that requires an annotation. *)

val declaration_mismatch : int -> Type.t -> Type.t -> 'a
(** [declaration_mismatch id expected actual] raises a {!DeclarationMismatch}
    exception. *)

val result_mismatch : Type.t -> Type.t -> 'a
(** [result_mismatch expected actual] raises a {!ResultMismatch} exception. *)

val unbound_identifier : int -> 'a
(** [unbound_identifier id] raises an {!UnboundIdentifier} exception. *)

val cannot_apply : Type.t -> 'a
(** [cannot_apply ty] raises a {!CannotApply} exception. *)

val invalid_args : Type.t -> Type.t -> 'a
(** [invalid_args expected actual] raises an {!InvalidArgs} exception. *)

val invalid_condition : Type.t -> 'a
(** [invalid_condition ty] raises an {!InvalidCondition} exception. *)

val conditional_branch_mismatch : Type.t -> Type.t -> 'a
(** [conditional_branch_mismatch t f] raises an {!ConditionalBranchMismatch}
    exception. *)

val annotation_required : int -> 'a
(** [annotation_required id] raises an {!AnnotationRequired} exception. *)
