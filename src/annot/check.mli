(** {1 Type Checking}
 *
 * Type checks the annotated source.
 *)

(** {2 Errors} *)

exception DeclarationMismatch of Patt.t * t * t
(** Raised when the declared type of a let-bound value does not match its actual
    type.  Contains the identifier, the expected type, and the actual type. *)

exception ResultMismatch of t * t
(** Raised when the declared result type of a let-bound function does not match
    the type of the function body.  Contains the expected and actual types. *)

exception UnboundIdentifier of Sym.sym
(** Raised when an identifier has not been bound and therefore has no type.
    Contains the identifier that was unbound. *)

exception CannotApply of t
(** Raised when a value that is not a function is applied to an argument.
    Contains the type of the value that was applied. *)

exception TooManyArgs of t * int
(** Raised when a function is applied to too many arguments.  Contains the type
    of the function and the number of arguments it was applied to. *)

exception InvalidArgs of t * t
(** Raised when a function is applied to an argument of invalid type.  Contains
    the expected and the actual argument types. *)

exception InvalidUnaryOperand of Type.t * un * Type.t
(** Raised when the type of a unary operand is invalid.  Contains the expected
    type, the operator, and the actual type. *)

exception InvalidBinaryOperands of Type.t * Type.t * bin * Type.t
(** Raised when the types of a binary operand is invalid.  Contains the expected
    type of the operands, the actual type of the left-hand operand, the
    operator, and the actual type of the right-hand operand. *)

exception InvalidEqualityOperands of Type.t * bin * Type.t
(** Raised when the types of an equality operand is invalid.  Contains the
    actual type of the left-hand operand, the operator, and the actual type of
    the right-hand operand. *)

(** {2 Patterns} *)

val irrefutable : Patt.t -> bool
(** [irrefutable patt] tests whether or not the pattern [patt] is irrefutable.
    That is, whether it will match all values of a type [ty] for which
    [matches_type patt ty] returns [true]. *)

(** {2 Operators} *)

val type_of_un : un -> Type.t -> Type.t
(** [type_of_un op r] determines the result type of the unary operator [op]
    applied to an argument of type [r].  Raises {!InvalidUnaryOperand} if [op]
    cannot be applied to an argument of type [r]. *)

val type_of_bin : Type.t -> bin -> Type.t -> Type.t
(** [type_of_bin l op r] determines the result type of the binary operator [op]
    applied to arguments of type [l] and [r].  Raises {!InvalidBinarOperand} if
    [op] cannot be applied to arguments of type [l] and [r]. *)

(** {2 Abstract Syntax} *)

val type_of_expr : Type.env -> Ast.expr -> Type.t
(** [type_of_expr env expr] type checks the expression [expr] in the type
    environment [env] and returns its type. *)

val type_of_top : Type.env -> Ast.top -> Type.env
(** [type_of_top env top] type checks the top-level expression [top] in the type
    environment [env] and returns its type along with a type environment with
    the top-level value bound. *)

val type_of_pkg : Type.env -> Ast.pkg -> Type.env
(** [type_of_pkg] type checks the package [pkg] in the type environment [env]
    and returns a type environment with all of the top-level values bound. *)
