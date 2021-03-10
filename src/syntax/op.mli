open Format

(** {1 Operators} *)

(** {2 Unary} *)

type un = private
  | Not of Loc.t (** Boolean negation *)
(** Unary Operators *)

(** {3 Constructors} *)

val un_not : Loc.t -> un
(** [un_not loc] constructs a unary boolean negation operator at location [loc].
    *)

(** {3 Operations} *)

val un_loc : un -> Loc.t
(** [un_loc op] returns the locatio of the unary operator [op]. *)

val un_precedence : un -> int
(** [un_precedence op] returns the operator precedence of the unary operator
    [op]. *)

(** {3 Type Checking} *)

exception InvalidUnaryOperand of Type.t * un * Type.t
(** Raised when the type of a unary operand is invalid.  Contains the expected
    type, the operator, and the actual type. *)

val type_of_un : un -> Type.t -> Type.t
(** [type_of_un op r] determines the result type of the unary operator [op]
    applied to an argument of type [r].  Raises {!InvalidUnaryOperand} if [op]
    cannot be applied to an argument of type [r]. *)

(** {2 Binary Operators} *)

type bin = private
  | Add of Loc.t  (** Addition *)
  | Sub of Loc.t  (** Subtraction *)
  | Mul of Loc.t  (** Multiplication *)
  | Div of Loc.t  (** Integer Division *)
  | Mod of Loc.t  (** Modulus *)
  | And of Loc.t  (** Logical And *)
  | Or of Loc.t   (** Logical Or *)
  | Eq of Loc.t   (** Equality *)
  | Neq of Loc.t  (** Inequality *)
  | Lte of Loc.t  (** Less Than or Equal *)
  | Lt of Loc.t   (** Less Than *)
  | Gt of Loc.t   (** Greater Than *)
  | Gte of Loc.t  (** Greater Than or Equal *)
  | Dot of Loc.t  (** Dot (Projection) *)
(** Binary Operators *)

(** {3 Constructors} *)

val bin_add : Loc.t -> bin
(** [bin_add loc] constructs a binary addition operator at location [loc]. *)

val bin_sub : Loc.t -> bin
(** [bin_sub loc] constructs a binary subtraction operator at location [loc]. *)

val bin_mul : Loc.t -> bin
(** [bin_mul loc] constructs a binary multiplication operator at location [loc].
    *)

val bin_div : Loc.t -> bin
(** [bin_div loc] constructs a binary integer division operator at location
    [loc]. *)

val bin_mod : Loc.t -> bin
(** [bin_mod loc] constructs a binary modulus operator at location [loc]. *)

val bin_and : Loc.t -> bin
(** [bin_and loc] constructs a binary logical "and" operator at location [loc].
    *)

val bin_or : Loc.t -> bin
(** [bin_or loc] constructs a binary logical "or" operator at location [loc]. *)

val bin_eq : Loc.t -> bin
(** [bin_eq loc] constructs a binary equality operator at location [loc]. *)

val bin_neq : Loc.t -> bin
(** [bin_neq loc] constructs a binary inequality operator at location [loc]. *)

val bin_lte : Loc.t -> bin
(** [bin_lte loc] constructs a binary less than or equal operator at location
    [loc]. *)

val bin_lt : Loc.t -> bin
(** [bin_lt loc] constructs a binary less than operator at location [loc]. *)

val bin_gt : Loc.t -> bin
(** [bin_gt loc] constructs a binary greater than operator at location [loc]. *)

val bin_gte : Loc.t -> bin
(** [bin_gte loc] constructs a binary greater than or equal operator at location
    [loc]. *)

val bin_dot : Loc.t -> bin
(** [bin_dot loc] constructs a binary dot (projection) operator at location
    [loc]. *)

(** {3 Operations} *)

val bin_loc : bin -> Loc.t
(** [bin_loc op] returns the location of the binary operator [op]. *)

val bin_precedence : bin -> int
(** [bin_precedence op] returns the operator precedence of the binary operator
    [op]. *)

(** {3 Type Checking} *)

exception InvalidBinaryOperands of Type.t * Type.t * bin * Type.t
(** Raised when the types of a binary operand is invalid.  Contains the expected
    type of the operands, the actual type of the left-hand operand, the
    operator, and the actual type of the right-hand operand. *)

exception InvalidEqualityOperands of Type.t * bin * Type.t
(** Raised when the types of an equality operand is invalid.  Contains the
    actual type of the left-hand operand, the operator, and the actual type of
    the right-hand operand. *)

val type_of_bin : Type.t -> bin -> Type.t -> Type.t
(** [type_of_bin l op r] determines the result type of the binary operator [op]
    applied to arguments of type [l] and [r].  Raises {!InvalidBinarOperand} if
    [op] cannot be applied to arguments of type [l] and [r]. *)
