open Format

(** {1 Operators} *)

(** {2 Unary} *)

type un = private
  | Not (** Boolean negation *)
(** Unary Operators *)

(** {3 Constructors} *)

val un_not : un
(** [un_not] constructs a unary boolean negation operator. *)

(** {3 Operations} *)

val un_precedence : un -> int
(** [un_precedence op] returns the operator precedence of the unary operator
    [op]. *)

(** {2 Binary Operators} *)

type bin = private
  | Add of Type.t  (** Addition *)
  | Sub of Type.t  (** Subtraction *)
  | Mul of Type.t  (** Multiplication *)
  | Div of Type.t  (** Integer Division *)
  | Mod            (** Modulus *)
  | And            (** Logical And *)
  | Or             (** Logical Or *)
  | Eq of Type.t   (** Equality *)
  | Neq of Type.t  (** Inequality *)
  | Lte of Type.t  (** Less Than or Equal *)
  | Lt of Type.t   (** Less Than *)
  | Gt of Type.t   (** Greater Than *)
  | Gte of Type.t  (** Greater Than or Equal *)
  | Cons of Type.t (** Cons *)
(** Binary Operators *)

(** {3 Constructors} *)

val bin_add : Type.t -> bin
(** [bin_add ty] constructs a binary addition operator for values of type [ty].
    *)

val bin_sub : Type.t -> bin
(** [bin_sub ty] constructs a binary subtraction operator for values of type
    [ty]. *)

val bin_mul : Type.t -> bin
(** [bin_mul ty] constructs a binary multiplication operator for values of type
    [ty]. *)

val bin_div : Type.t -> bin
(** [bin_div ty] constructs a binary integer division operator for values of
    type [ty]. *)

val bin_mod : bin
(** [bin_mod] constructs a binary modulus operator. *)

val bin_and : bin
(** [bin_and] constructs a binary logical "and" operator. *)

val bin_or : bin
(** [bin_or] constructs a binary logical "or" operator. *)

val bin_eq : Type.t -> bin
(** [bin_eq ty] constructs a binary equality operator for values of type [ty].
    *)

val bin_neq : Type.t -> bin
(** [bin_neq ty] constructs a binary inequality operator for values of type
    [ty]. *)

val bin_lte : Type.t -> bin
(** [bin_lte ty] constructs a binary less than or equal operator for values of
    type [ty]. *)

val bin_lt : Type.t -> bin
(** [bin_lt ty] constructs a binary less than operator for values of type
    [ty]. *)

val bin_gt : Type.t -> bin
(** [bin_gt ty] constructs a binary greater than operator for values of type
    [ty]. *)

val bin_gte : Type.t -> bin
(** [bin_gte ty] constructs a binary greater than or equal operator for values
    of type [ty]. *)

val bin_cons : Type.t -> bin
(** [bin_cons ty] constructs a binary cons operator for values of type [ty]. *)

(** {3 Operations} *)

val bin_precedence : bin -> int
(** [bin_precedence op] returns the operator precedence of the binary operator
    [op]. *)
