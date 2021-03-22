open Common

(** {1 Operators} *)

(** {2 Unary} *)

type un = private
  | LNot of {
      loc: Loc.t (** Location Tracking Information *)
    }(** Logical NOT *)
  | BNot of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Bitwise NOT *)
(** Unary Operators *)

(** {3 Constructors} *)

val un_lnot : Loc.t -> un
(** [un_lnot loc] constructs a logical NOT operator at location [loc]. *)

val un_bnot : Loc.t -> un
(** [un_bnot loc] constructs a bitwise NOT operator at location [loc]. *)

(** {2 Binary Operators} *)

type bin = private
  | BAnd of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Bitwise AND *)
  | BOr of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Bitwise OR *)
  | BXor of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Bitwise XOR *)
  | Lsl of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Logical Shift Left *)
  | Lsr of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Logical Shift Right *)
  | Asl of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Arithmetic Shift Left *)
  | Asr of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Arithmetic Shift Right *)
  | Add of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Addition *)
  | Sub of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Subtraction *)
  | Mul of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Multiplication *)
  | Div of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Integer Division *)
  | Mod of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Modulus *)
  | LAnd of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Logical AND *)
  | LOr of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Logical OR *)
  | Eq of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Equality *)
  | Neq of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Inequality *)
  | Lte of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Less Than or Equal *)
  | Lt of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Less Than *)
  | Gt of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Greater Than *)
  | Gte of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Greater Than or Equal *)
  | Concat of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Concatenation *)
  | Cons of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Cons *)
(** Binary Operators *)

(** {3 Constructors} *)

val bin_band : Loc.t -> bin
(** [bin_band loc] constructs a bitwise AND operator at location [loc]. *)

val bin_bor : Loc.t -> bin
(** [bin_bor loc] constructs a bitwise OR operator at location [loc]. *)

val bin_bxor : Loc.t -> bin
(** [bin_bxor loc] constructs a bitwise XOR operator at location [loc]. *)

val bin_lsl : Loc.t -> bin
(** [bin_lsl loc] constructs a logical shift left operator at location [loc]. *)

val bin_lsr : Loc.t -> bin
(** [bin_lsr loc] constructs a logical shift right operator at location [loc].
    *)

val bin_asl : Loc.t -> bin
(** [bin_asl loc] constructs a arithmetic shift left operator at location [loc].
    *)

val bin_asr : Loc.t -> bin
(** [bin_asr loc] constructs a arithmetic shift right operator at location
    [loc]. *)

val bin_add : Loc.t -> bin
(** [bin_add loc] constructs a addition operator at location [loc]. *)

val bin_sub : Loc.t -> bin
(** [bin_sub loc] constructs a subtraction operator at location [loc]. *)

val bin_mul : Loc.t -> bin
(** [bin_mul loc] constructs a multiplication operator at location [loc]. *)

val bin_div : Loc.t -> bin
(** [bin_div loc] constructs a division operator at location [loc]. *)

val bin_mod : Loc.t -> bin
(** [bin_mod loc] constructs a modulus operator at location [loc]. *)

val bin_land : Loc.t -> bin
(** [bin_and loc] constructs a logical AND operatorat location [loc]. *)

val bin_lor : Loc.t -> bin
(** [bin_or loc] constructs a logical OR operatorat location [loc]. *)

val bin_eq : Loc.t -> bin
(** [bin_eq loc] constructs a equality operator at location [loc]. *)

val bin_neq : Loc.t -> bin
(** [bin_neq loc] constructs a inequality operator at location [loc]. *)

val bin_lte : Loc.t -> bin
(** [bin_lte loc] constructs a less than or equal operator at location [loc]. *)

val bin_lt : Loc.t -> bin
(** [bin_lt loc] constructs a less than operator at location [loc]. *)

val bin_gt : Loc.t -> bin
(** [bin_gt loc] constructs a greater than operator at location [loc]. *)

val bin_gte : Loc.t -> bin
(** [bin_gte loc] constructs a greater than or equal operator at location [loc].
    *)

val bin_concat : Loc.t -> bin
(** [bin_concat loc] constructs a concatenation operator at location [loc].
    *)

val bin_cons : Loc.t -> bin
(** [bin_cons loc] constructs a cons operator at location [loc]. *)
