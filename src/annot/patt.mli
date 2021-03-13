open Format

(** {1 Patterns} *)

type t =
  | Unit (** Unit *)
  | Bool of {
      b: bool (** Value *)
    } (** Boolean *)
  | Int of {
      i: int (** Value *)
    } (** Integer *)
  | Float of {
      f: float (** Value *)
    } (** Floating-point *)
  | Rune of {
      r: bytes (** Value *)
    } (** Rune *)
  | String of {
      s: string (** Value *)
    } (** String *)
  | Byte of {
      r: bytes (** Value *)
    } (** Blob *)
  | Blob of {
      s: string (** Value *)
    } (** Blob *)
  | Timestamp of {
      ts: string (** Value *)
    } (** Timestamp *)
  | Duration of {
      d: string (** Value *)
    } (** Duration *)
  | Ground of {
      ty: Type.t (** Type *)
    }(** Ground *)
  | Var of {
      id: Sym.sym; (** Name *)
      ty: Type.t   (** Type *)
    } (** Variable *)
  | Tuple of {
      arity: int;    (** Arity *)
      patts: t list; (** Element Patterns *)
      ty:    Type.t  (** Type *)
    } (** Tuple *)
  | Record of {
      fields: field list; (** Field Patterns *)
      ty:     Type.t      (** Type *)
    } (** Record *)
  | Or of {
      patts: t list (** Patterns *)
    } (** Or *)
(** A pattern *)

and field = Field of {
  name: Sym.sym; (** Field Name *)
  patt: t        (** Value Pattern *)
}
(** Record field *)

(** {2 Constructors} *)

val unit : t
(** [unit] constructs a unit pattern. *)

val bool : bool -> t
(** [bool b] constructs a boolean pattern matching [b]. *)

val int : int -> t
(** [int i] constructs an integer pattern matching [i]. *)

val float : float -> t
(** [float f] constructs a floating-point pattern matching [f].*)

val rune : bytes -> t
(** [rune r] constructs a rune pattern matching [r]. *)

val string : string -> t
(** [string s] constructs a string pattern matching [s]. *)

val byte : bytes -> t
(** [byte b] constructs a byte pattern matching [b]. *)

val blob : bytes -> t
(** [blob bs] constructs a blob pattern matching [bs]. *)

val timestamp : string -> t
(** [timestamp ts] constructs a timestamp pattern matching [ts]. *)

val duration : string -> t
(** [duration d] constructs a duration pattern matching [d]. *)

val ground : Type.t -> t
(** [ground ty] constructs a ground pattern matching values of type [ty]. *)

val var : Sym.sym -> Type.t -> t
(** [var sym ty] constructs a variable pattern matching values of type [ty] and
    binding the matched value to [sym]. *)

val tuple : t list -> Type.t -> t
(** [tuple patts ty] constructs a tuple pattern matching a tuple of type [ty]
    when the element patterns [patts] match their corresponding elements. *)

val record : field list -> Type.t -> t
(** [record fields] constructs a record pattern matching a record of type [ty]
    when the field patterns [fields] match their corresponding fields. *)

val orr : t list -> t
(** [orr patts] constructs an "or" pattern matching any of the patters [patts].
    *)

val field : Sym.sym -> t -> field
(** [field name patt] constructs a pattern that matches a record field. The
    value of the field must match [patt]. *)
