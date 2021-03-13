open Format

(** {1 Patterns} *)

type atom =
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
      length: int;   (** Length *)
      s:      string (** Value *)
    } (** String *)
  | Byte of {
      r: bytes (** Value *)
    } (** Rune *)
  | Blob of {
      length: int;   (** Length *)
      s:      bytes (** Value *)
    } (** String *)
  | Timestamp of {
      ts: string (** Value *)
    } (** Timestamp *)
  | Duration of {
      d: string (** Value *)
    } (** Duration *)
  | Var of {
      id: Sym.sym; (** Variable Name *)
      ty: Type.t   (** Type *)
    } (** Variable *)
  | Ground of {
      ty: Type.t
    } (** Ground *)

type compound =
  | Constr of {
      name: Sym.sym; (** Constructor Name *)
      arg:  atom     (** Argument *)
    } (** Constructor *)
  | Atom of {
      atom: atom (** Pattern *)
    } (** Atomic Pattern *)
(** Patterns *)

(** {2 Constructors} *)

val unit : atom
(** [unit] constructs a unit pattern. *)

val bool : bool -> atom
(** [bool b] constructs a boolean pattern matching [b]. *)

val int : int -> atom
(** [int i] constructs an integer pattern matching [i]. *)

val float : float -> atom
(** [float f] constructs a floating-point pattern matching [f].*)

val rune : bytes -> atom
(** [rune r] constructs a rune pattern matching [r]. *)

val string : string -> atom
(** [string s] constructs a string pattern matching [s]. *)

val byte : bytes -> atom
(** [byte b] constructs a byte pattern matching [b]. *)

val blob : bytes -> atom
(** [blob bs] constructs a binary large object (BLOB) pattern matching [bs]. *)

val timestamp : string -> atom
(** [timestamp ts] constructs a timestamp pattern matching [ts]. *)

val duration : string -> atom
(** [duration d] constructs a duration pattern matching [d]. *)

val var : Sym.sym -> Type.t -> atom
(** [var id ty] constructs a variable pattern binding the matched value of type
    [ty] to [id]. *)

val ground : Type.t -> atom
(** [ground ty] constructs a ground pattern maching all values of type [ty]. *)

val constr : Sym.sym -> atom -> compound
(** [constr name args] constructs a variant constructor pattern matching the
    constructor [name] with the argument patterns [args]. *)

val atom : atom -> compound
(** [atom atom] constructs an atomic pattern matching [atom]. *)
