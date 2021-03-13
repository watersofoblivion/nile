open Format

(** {1 Operators} *)

(** {2 Unary} *)

type un = private
  | LNot (** Logical NOT *)
  | BNot of {
      ty: Type.t (** Type *)
    } (** Bitwise NOT *)
(** Unary Operators *)

(** {3 Constructors} *)

val un_lnot : un
(** [un_lnot] constructs a logical NOT operator. *)

val un_bnot : Type.t -> un
(** [un_bnot ty] constructs a bitwise NOT operator for values of type [ty]. *)

(** {2 Binary Operators} *)

type bin = private
  | BAnd of {
      ty: Type.t (** Type *)
    } (** Bitwise AND *)
  | BOr of {
      ty: Type.t (** Type *)
    } (** Bitwise OR *)
  | BXor of {
      ty: Type.t (** Type *)
    } (** Bitwise XOR *)
  | Lsl of {
      ty: Type.t (** Type *)
    } (** Logical Shift Left *)
  | Lsr of {
      ty: Type.t (** Type *)
    } (** Logical Shift Right *)
  | Asl of {
      ty: Type.t (** Type *)
    } (** Arithmetic Shift Left *)
  | Asr of {
      ty: Type.t (** Type *)
    } (** Arithmetic Shift Right *)
  | Add of {
      ty: Type.t (** Type *)
    } (** Addition *)
  | Sub of {
      ty: Type.t (** Type *)
    } (** Subtraction *)
  | Mul of {
      ty: Type.t (** Type *)
    } (** Multiplication *)
  | Div of {
      ty: Type.t (** Type *)
    } (** Integer Division *)
  | Mod (** Modulus *)
  | LAnd (** Logical AND *)
  | LOr (** Logical OR *)
  | Eq of {
      ty: Type.t (** Type *)
    } (** Equality *)
  | Neq of {
      ty: Type.t (** Type *)
    } (** Inequality *)
  | Lte of {
      ty: Type.t (** Type *)
    } (** Less Than or Equal *)
  | Lt of {
      ty: Type.t (** Type *)
    } (** Less Than *)
  | Gt of {
      ty: Type.t (** Type *)
    } (** Greater Than *)
  | Gte of {
      ty: Type.t (** Type *)
    } (** Greater Than or Equal *)
  | Concat of {
      ty: Type.t (** Type *)
    } (** Concatenation *)
  | Cons of {
      ty: Type.t (** Type *)
    } (** Cons *)
(** Binary Operators *)

(** {3 Constructors} *)

val bin_band : Type.t -> bin
(** [bin_band ty] constructs a bitwise AND operator for values of type [ty]. *)

val bin_bor : Type.t -> bin
(** [bin_bor ty] constructs a bitwise OR operator for values of type [ty]. *)

val bin_bxor : Type.t -> bin
(** [bin_bxor ty] constructs a bitwise XOR operator for values of type [ty]. *)

val bin_lsl : Type.t -> bin
(** [bin_lsl ty] constructs a logical shift left operator for values of type
    [ty]. *)

val bin_lsr : Type.t -> bin
(** [bin_lsr ty] constructs a logical shift right operator for values of type
    [ty]. *)

val bin_asl : Type.t -> bin
(** [bin_asl ty] constructs a arithmetic shift left operator for values of type
    [ty]. *)

val bin_asr : Type.t -> bin
(** [bin_asr ty] constructs a arithmetic shift right operator for values of type
    [ty]. *)

val bin_add : Type.t -> bin
(** [bin_add ty] constructs a addition operator for values of type [ty]. *)

val bin_sub : Type.t -> bin
(** [bin_sub ty] constructs a subtraction operator for values of type [ty]. *)

val bin_mul : Type.t -> bin
(** [bin_mul ty] constructs a multiplication operator for values of type [ty].
    *)

val bin_div : Type.t -> bin
(** [bin_div ty] constructs a division operator for values of type [ty]. *)

val bin_mod : bin
(** [bin_mod] constructs a modulus operator. *)

val bin_land : bin
(** [bin_and] constructs a logical AND operator. *)

val bin_lor : bin
(** [bin_or] constructs a logical OR operator. *)

val bin_eq : Type.t -> bin
(** [bin_eq ty] constructs a equality operator for values of type [ty]. *)

val bin_neq : Type.t -> bin
(** [bin_neq ty] constructs a inequality operator for values of type [ty]. *)

val bin_lte : Type.t -> bin
(** [bin_lte ty] constructs a less than or equal operator for values of type
    [ty]. *)

val bin_lt : Type.t -> bin
(** [bin_lt ty] constructs a less than operator for values of type [ty]. *)

val bin_gt : Type.t -> bin
(** [bin_gt ty] constructs a greater than operator for values of type [ty]. *)

val bin_gte : Type.t -> bin
(** [bin_gte ty] constructs a greater than or equal operator for values of type
    [ty]. *)

val bin_concat : Type.t -> bin
(** [bin_concat ty] constructs a concatenation operator for values of type [ty].
    *)

val bin_cons : Type.t -> bin
(** [bin_cons ty] constructs a cons operator for values of type [ty]. *)
