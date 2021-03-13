(** {1 Type Checking}
 *
 * Type checks the ANF IR.
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

exception InvalidCondition of t
(** Raised when the condition clause of a conditional statement is not a boolean
    typed value.  Contains the actual type of the condition clause. *)

(** {2 Builtins} *)

val type_of_builtin : Anf.builtin -> Type.t

(** {2 Abstract Syntax Trees} *)

val type_of_atom : Type.env -> atom -> Type.t
(** [type_of_atom env atom] type checks the atomic value [atom] in the type
    environment [env] and results in the type of the atomic value. *)

val type_of_expr : Type.env -> expr -> Type.t
(** [type_of_expr env expr] type checks the primitive expression [expr] in the
    type environment [env] and results in the type of the primitive expression.
    *)

val type_of_block : Type.env -> block -> Type.t
(** [type_of_block env block] type checks the block expression [block] in the
    type environment [env] and results in the type of the block expression. *)

val type_of_top : Type.env -> top -> Type.env
(** [type_of_top env top] type checks the top-level expression [top] in the
    type environment [env] and results in a type environment with the top-level
    identifiers bound to their types. *)

val type_of_file : Type.env -> file -> Type.env
(** [type_of_file env file] type checks the source file [file] in the type
    environment [env] and returns a type environment with all top-level
    identifiers bound to their types. *)
