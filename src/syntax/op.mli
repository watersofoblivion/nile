open Format

(** {1 Unary Operators} *)

type un = private
  | Not of Loc.t (** Boolean negation *)
(** Unary Operators *)

(** {2 Constructors} *)

val un_not : Loc.t -> un
(** [un_not loc] constructs a unary boolean negation operator. *)

(** {2 Operations} *)

val pp_un : un -> formatter -> unit
(** [pp_un op fmt] pretty-prints [op] to the formatter [fmt]. *)

val un_precedence : un -> int
(** [un_precedence op] returns the operator precedence of the unary operator
    [op]. *)

val un_loc : un -> Loc.t
(** [un_loc op] returns the location information associated with the unary
    operator [op]. *)

(** {1 Binary Operators} *)

type bin = private
  | Add of Loc.t (** Addition *)
  | Sub of Loc.t (** Subtraction *)
  | Mul of Loc.t (** Multiplication *)
  | Div of Loc.t (** Integer Division *)
  | Mod of Loc.t (** Modulus *)
  | And of Loc.t (** Logical And *)
  | Or of Loc.t (** Logical Or *)
  | Eq of Loc.t (** Equality *)
  | Neq of Loc.t (** Inequality *)
  | Lte of Loc.t (** Less Than or Equal *)
  | Lt of Loc.t (** Less Than *)
  | Gt of Loc.t (** Greater Than *)
  | Gte of Loc.t (** Greater Than or Equal *)
(** Binary Operators *)

(** {2 Constructors} *)

val bin_add : Loc.t -> bin
(** [bin_add loc] constructs a binary addition operator. *)

val bin_sub : Loc.t -> bin
(** [bin_sub loc] constructs a binary subtraction operator. *)

val bin_mul : Loc.t -> bin
(** [bin_mul loc] constructs a binary multiplication operator. *)

val bin_div : Loc.t -> bin
(** [bin_div loc] constructs a binary integer division operator. *)

val bin_mod : Loc.t -> bin
(** [bin_mod loc] constructs a binary modulus operator. *)

val bin_and : Loc.t -> bin
(** [bin_and loc] constructs a binary logical "and" operator. *)

val bin_or : Loc.t -> bin
(** [bin_or loc] constructs a binary logical "or" operator. *)

val bin_eq : Loc.t -> bin
(** [bin_eq loc] constructs a binary equality operator. *)

val bin_neq : Loc.t -> bin
(** [bin_neq loc] constructs a binary inequality operator. *)

val bin_lte : Loc.t -> bin
(** [bin_lte loc] constructs a binary less than or equal operator. *)

val bin_lt : Loc.t -> bin
(** [bin_lte loc] constructs a binary less than operator. *)

val bin_gt : Loc.t -> bin
(** [bin_lte loc] constructs a binary greater than operator. *)

val bin_gte : Loc.t -> bin
(** [bin_lte loc] constructs a binary greater than or equal operator. *)

(** {2 Operations} *)

val pp_bin : bin -> formatter -> unit
(** [pp_bin op fmt] pretty-prints [op] to the formatter [fmt]. *)

val bin_precedence : bin -> int
(** [bin_precedence op] returns the operator precedence of the binary operator
    [op]. *)

val bin_loc : bin -> Loc.t
(** [bin_loc op] returns the location information associated with the binary
    operator [op]. *)
