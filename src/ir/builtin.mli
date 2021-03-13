(** {1 Built-in Functions} *)

type t = private
  | LAnd (** Logical AND *)
  | LOr  (** Logical OR *)
  | LNot (** Logical NOT *)
  | BAnd of {
      ty: Type.t (** Operand Type *)
    } (** Bitwise AND *)
  | BOr of {
      ty: Type.t (** Operand Type *)
    } (** Bitwise OR *)
  | BXor of {
      ty: Type.t (** Operand Type *)
    } (** Bitwise XOR *)
  | BNot of {
      ty: Type.t (** Operand Type *)
    } (** Bitwise NOT *)
  | Lsl of {
      ty: Type.t (** Operand Type *)
    } (** Logical Shift Left *)
  | Lsr of {
      ty: Type.t (** Operand Type *)
    } (** Logical Shift Right *)
  | Asl of {
      ty: Type.t (** Operand Type *)
    } (** Arithmetic Shift Left *)
  | Asr of {
      ty: Type.t (** Operand Type *)
    } (** Arithmetic Shift Right *)
  | Eq of {
      ty: Type.t (** Operand Type *)
    } (** Equality *)
  | Neq of {
      ty: Type.t (** Operand Type *)
    } (** Inequality *)
  | Add of {
      ty: Type.t (** Operand Type *)
    } (** Addition *)
  | Sub of {
      ty: Type.t (** Operand Type *)
    } (** Subtraction *)
  | Mul of {
      ty: Type.t (** Operand Type *)
    } (** Multiplication *)
  | Div of {
      ty: Type.t (** Operand Type *)
    } (** Division *)
  | Mod of {
      ty: Type.t (** Operand Type *)
    } (** Modulus *)
  | Lte of {
      ty: Type.t (** Operand Type *)
    } (** Less Than or Equals *)
  | Lt of {
      ty: Type.t (** Operand Type *)
    } (** Less Than *)
  | Gt of {
      ty: Type.t (** Operand Type *)
    } (** Greater Than *)
  | Gte of {
      ty: Type.t (** Operand Type *)
    } (** Greater Than or Equal *)
  | Slice of {
      ty: Type.t (** Operand Type *)
    } (** Slice *)
  | Index of {
      ty: Type.t (** Operand Type *)
    } (** Indexing *)
  | Concat of {
      ty: Type.t (** Operand Type *)
    } (** Concatenation *)
  | Before (** Before Timestamp *)
  | After  (** After Timestamp *)

(** {2 Constructors} *)

(** {3 Logical} *)

val log_and : t
(** [log_and] constructs a logical AND builtin. *)

val log_or : t
(** [log_or] constructs a logical OR builtin. *)

val log_not : t
(** [log_not] constructs a logical NOT builtin. *)

(** {3 Bitwise} *)

val bit_and : Type.t -> t
(** [bit_and ty] constructs a bitwise AND operator on values of type [ty]. *)

val bit_or : Type.t -> t
(** [bit_or ty] constructs a bitwise OR operator on values of type [ty]. *)

val bit_xor : Type.t -> t
(** [bit_xor ty] constructs a bitwise exclusive-OR operator on values of type
    [ty]. *)

val bit_not : Type.t -> t
(** [bit_not ty] constructs a bitwise NOT operator on values of type [ty]. *)

(** {3 Shifts} *)

val log_shl : Type.t -> t
(** [log_shl ty] constructs a logical shift left operator on values of type
    [ty]. *)

val log_shr : Type.t -> t
(** [log_shr ty] constructs a logical shift right operator on values of type
    [ty]. *)

val arith_shl : Type.t -> t
(** [arith_shl ty] constructs an arithmetical shift left operator on values of
    type [ty]. *)

val arith_shr : Type.t -> t
(** [arith_shr ty] constructs an arithmetical shift right operator on values of
    type [ty]. *)

(** {3 Equality} *)

val eq : Type.t -> t
(** [eq ty] constructs an equality operator on values of type [ty]. *)

val neq : Type.t -> t
(** [neq ty] constructs an inequality operator on values of type [ty]. *)

(** {3 Arithmetic} *)

val add : Type.t -> t
(** [add ty] constructs an addition operator on values of type [ty]. *)

val sub : Type.t -> t
(** [sub ty] constructs a subtraction operator on values of type [ty]. *)

val mul : Type.t -> t
(** [mul ty] constructs a multiplication operator on values of type [ty]. *)

val div : Type.t -> t
(** [div ty] constructs a division operator on values of type [ty]. *)

val modulus : Type.t -> t
(** [modulus ty] constructs a modulus operator on values of type [ty]. *)

(** {3 Comparison} *)

val lte : Type.t -> t
(** [lte ty] constructs a less than or equal operator on values of type [ty]. *)

val lt : Type.t -> t
(** [lt ty] constructs a less than operator on values of type [ty]. *)

val gt : Type.t -> t
(** [gt ty] constructs a greater than operator on values of type [ty]. *)

val gte : Type.t -> t
(** [gte ty] constructs a greater than or equal operator on values of type [ty].
    *)

(** {3 Sequences} *)

val slice : Type.t -> t
(** [slice ty] constructs a slice operator on values of type [ty]. *)

val index : Type.t -> t
(** [index ty] constructs a index operator on values of type [ty]. *)

val concat : Type.t -> t
(** [concat ty] constructs a concatenation operator on values of type [ty]. *)

(** {3 Temporal} *)

val before : t
(** [before] constructs a before operator that subtracts a duration from a
    timestamp. *)

val after : t
(** [after] constructs an after operator that adds a duration from a
    timestamp. *)
