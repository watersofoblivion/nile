(** {1 Type Checking} *)

(** {2 Environment} *)

type env
(** A type environment *)

val env : env
(** [env] constructs an empty type environment. *)

val bind : string -> Type.t -> env -> env
(** [bind id ty env] constructs copy of [env] extended by binding the type [ty]
    to the identifier [id].  Any type previously bound to [id] is masked.  The
    original environment is left unaltered. *)

val lookup : string -> env -> Type.t
(** [lookup id env] finds the type bound to the identifier [id] in the
    environment [env], or raises {!Not_found} if the identifier is unbound. *)

(** {2 Errors} *)

exception InvalidUnaryOperand of Type.t * Op.un * Type.t
(** Raised when the type of a unary operand is invalid.  Contains the expected
    type, the operator, and the actual type. *)

exception InvalidBinaryOperands of Type.t * Type.t * Op.bin * Type.t
(** Raised when the types of a binary operand is invalid.  Contains the expected
    type of the operands, the actual type of the left-hand operand, the
    operator, and the actual type of the right-hand operand. *)

exception InvalidEqualityOperands of Type.t * Op.bin * Type.t
(** Raised when the types of an equality operand is invalid.  Contains the
    actual type of the left-hand operand, the operator, and the actual type of
    the right-hand operand. *)

exception DeclarationMismatch of string * Type.t * Type.t
(** Raised when the declared type of a let-bound value does not match its actual
    type.  Contains the identifier, the expected type, and the actual type. *)

exception ResultMismatch of Type.t * Type.t
(** Raised when the declared result type of a let-bound function does not match
    the type of the function body.  Contains the expected and actual types. *)

exception UnboundIdentifier of string
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

(** {2 Checker} *)

val type_of : env -> Ast.t -> Type.t
(** [type_of env ast] returns the type of [ast] within the type environment
    [env] or raises an exception if [ast] is incorrectly typed. *)
