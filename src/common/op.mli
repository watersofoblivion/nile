open Format

(** {1 Unary Operators} *)

type un = private
  | Not (** Boolean negation *)
(** Unary Operators *)

(** {2 Constructors} *)

val un_not : un
(** [un_not] constructs a unary boolean negation operator. *)

(** {2 Operations} *)

val pp_un : un -> formatter -> unit
(** [pp_un op fmt] pretty-prints [op] to the formatter [fmt]. *)

val un_precedence : un -> int
(** [un_precedence op] returns the operator precedence of the unary operator
    [op]. *)

(** {1 Binary Operators} *)

type bin = private
  | Add (** Addition *)
  | Sub (** Subtraction *)
  | Mul (** Multiplication *)
  | Div (** Integer Division *)
  | Mod (** Modulus *)
  | And (** Logical And *)
  | Or (** Logical Or *)
  | Eq (** Equality *)
  | Neq (** Inequality *)
  | Lte (** Less Than or Equal *)
  | Lt (** Less Than *)
  | Gt (** Greater Than *)
  | Gte (** Greater Than or Equal *)
(** Binary Operators *)

(** {2 Constructors} *)

val bin_add : bin
(** [bin_add] constructs a binary addition operator. *)

val bin_sub : bin
(** [bin_sub] constructs a binary subtraction operator. *)

val bin_mul : bin
(** [bin_mul] constructs a binary multiplication operator. *)

val bin_div : bin
(** [bin_div] constructs a binary integer division operator. *)

val bin_mod : bin
(** [bin_mod] constructs a binary modulus operator. *)

val bin_and : bin
(** [bin_and] constructs a binary logical "and" operator. *)

val bin_or : bin
(** [bin_or] constructs a binary logical "or" operator. *)

val bin_eq : bin
(** [bin_eq] constructs a binary equality operator. *)

val bin_neq : bin
(** [bin_neq] constructs a binary inequality operator. *)

val bin_lte : bin
(** [bin_lte] constructs a binary less than or equal operator. *)

val bin_lt : bin
(** [bin_lte] constructs a binary less than operator. *)

val bin_gt : bin
(** [bin_lte] constructs a binary greater than operator. *)

val bin_gte : bin
(** [bin_lte] constructs a binary greater than or equal operator. *)

(** {2 Operations} *)

val pp_bin : bin -> formatter -> unit
(** [pp_bin op fmt] pretty-prints [op] to the formatter [fmt]. *)

val bin_precedence : bin -> int
(** [bin_precedence op] returns the operator precedence of the binary operator
    [op]. *)
