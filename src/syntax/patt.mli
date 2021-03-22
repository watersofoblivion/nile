open Common

(** {1 Patterns} *)

type t =
  | Unit of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Unit *)
  | Bool of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** Boolean *)
  | Int of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      radix:  int     (** Radix *)
    } (** Integer *)
  | Float of {
      loc:    Loc.t;  (** Location Tracking Information *)
      lexeme: string; (** Lexeme *)
      hex:    bool    (** Hexadecimal format *)
    } (** Floating-point *)
  | Rune of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** Rune *)
  | String of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** String *)
  | Byte of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** Byte *)
  | Blob of {
      loc:    Loc.t; (** Location Tracking Information *)
      lexeme: string (** Lexeme *)
    } (** Byte *)
  | Timestamp of {
      loc:         Loc.t;         (** Location Tracking Information *)
      lexeme:      string;        (** Raw Lexeme *)
      year:        string option; (** Year Lexeme *)
      month:       string option; (** Month Lexeme *)
      week:        string option; (** Week Lexeme *)
      day:         string option; (** Day Lexeme *)
      ordinal:     string option; (** Ordinal Lexeme *)
      hour:        string option; (** Hour Lexeme *)
      minute:      string option; (** Minute Lexeme *)
      second:      string option; (** Second Lexeme *)
      fraction:    string option; (** Fractional Lexeme *)
      off_sign:    string option; (** Offset Sign *)
      off_hours:   string option; (** Offset Hours Lexeme *)
      off_minutes: string option; (** Offset Minutes Lexeme *)
    } (** Timestamp *)
  | Duration of {
      loc:      Loc.t;         (** Location Tracking Information *)
      lexeme:   string;        (** Raw Lexeme *)
      years:    string option; (** Years Lexeme *)
      months:   string option; (** Months Lexeme *)
      weeks:    string option; (** Weeks Lexeme *)
      days:     string option; (** Days Lexeme *)
      hours:    string option; (** Hours Lexeme *)
      minutes:  string option; (** Minutes Lexeme *)
      seconds:  string option; (** Seconds Lexeme *)
      fraction: string option  (** Fractional Lexeme *)
    } (** Duration *)
  | Ground of {
      loc: Loc.t (** Location Tracking Information *)
    } (** Ground *)
  | Var of {
      loc: Loc.t;  (** Location Tracking Information *)
      id:  Sym.sym (** Identifier *)
    } (** Variable *)
  | Tuple of {
      loc:   Loc.t; (** Location Tracking Information *)
      arity: int;   (** Arity *)
      patts: t list (** Element Patterns *)
    } (** Tuple *)
  | Record of {
      loc:     Loc.t;      (** Location Tracking Information *)
      fields:  field list; (** Field Patterns *)
      elipsis: bool        (** Elipsis *)
    } (** Record *)
  | Constr of {
      loc:  Loc.t;        (** Location Tracking Information *)
      name: Sym.sym list; (** Constructor Name *)
      args: t list        (** Argument Patterns *)
    } (** Variant Constructor *)
  | Or of {
      loc:  Loc.t;  (** Location Tracking Information *)
      patts: t list (** Patterns *)
    } (** Or *)
(** A pattern *)

and field =
  | Bare of {
      loc: Loc.t;   (** Location Tracking Information *)
      name: Sym.sym (** Field Name *)
    } (** Bare field name *)
  | Named of {
      loc:  Loc.t;   (** Location Tracking Information *)
      name: Sym.sym; (** Field Name *)
      patt: t        (** Value Pattern *)
    } (** Named field *)
(** Record field *)

(** {2 Constructors} *)

val unit : Loc.t -> t
(** [unit loc] constructs a unit pattern at location [loc]. *)

val bool : Loc.t -> string -> t
(** [bool loc lexeme] constructs a boolean pattern matching [lexeme] at location
    [loc]. *)

val int : Loc.t -> string -> int -> t
(** [int loc lexeme radix] constructs an integer pattern matching [lexeme] with
    radix [radix] at location [loc]. *)

val float : Loc.t -> string -> bool -> t
(** [float loc lexeme hex] constructs a floating-point pattern matching [lexeme]
    at location [loc].  If [hex] is true, the lexeme is in hexadecimal format.
    *)

val rune : Loc.t -> string -> t
(** [rune loc lexeme] constructs a rune pattern matching [lexeme] at location
    [loc].  The lexeme should include the single quotes. *)

val string : Loc.t -> string -> t
(** [string loc lexeme] constructs a string pattern matching [lexeme] at
    location [loc].  The lexeme should include the double quotes. *)

val byte : Loc.t -> bytes -> t
(** [byte loc b] constructs a byte pattern matching [b] at location [loc].  The
    lexeme should include the [\<radix>] prefix. *)

val blob : Loc.t -> bytes -> t
(** [blob loc bs] constructs a binary large object (BLOB) pattern matching [bs]
    at location [loc].  The lexeme should include the back quotes. *)

val timestamp : Loc.t -> string -> t
(** [timestamp loc lexeme] constructs a ISO-8601 timestamp pattern matching the
    lexeme [lexeme] at location [loc].  The lexeme should include the [@]
    prefix. *)

val duration : Loc.t -> string -> t
(** [duration loc lexeme] constructs a ISO-8601 duration pattern matching the
    lexeme [lexeme] at location [loc].  The lexeme should include the [@@]
    prefix. *)

val ground : Loc.t -> t
(** [ground loc] constructs a ground pattern at location [loc]. *)

val var : Loc.t -> Sym.sym -> t
(** [var loc id] constructs a variable pattern at location [loc] binding the
    matched value to [id]. *)

val tuple : Loc.t -> t list -> t
(** [tuple loc patts] constructs a tuple pattern at location [loc] matching a
    tuple when the element patterns [patts] match their corresponding elements.
    *)

val record : Loc.t -> field list -> bool -> t
(** [record loc fields elipsis] constructs a record pattern at location [loc]
    matching a record when the field patterns [fields] match their corresponding
    fields.  If [elipsis] is [true], then all fields of the record not provided
    are matched against the ground ("[_]") pattern. *)

val constr : Loc.t -> Sym.sym list -> t list -> t
(** [constr loc name args] constructs a variant construction pattern located at
    [loc] matching the constructor [name] with value [args]. *)

val orr : Loc.t -> t list -> t
(** [orr loc patts] constructs an "or" pattern at location [loc] matching any of
    the patters [patts]. *)

val bare : Loc.t -> Sym.sym -> field
(** [bare loc name] constructs a pattern at location [loc] that matches a record
    field. The value of the field is matched to a variable named identically to
    the field. *)

val named : Loc.t -> Sym.sym -> t -> field
(** [named loc name patt] constructs a pattern at location [loc] that matches a
    record field. The value of the field must match [patt]. *)

(** {2 Operations} *)

val loc : t -> Loc.t
(** [loc patt] returns the location of the pattern [patt]. *)

(** {2 Type Checking} *)

val irrefutable : t -> bool
(** [irrefutable patt] tests whether or not the pattern [patt] is irrefutable.
    That is, whether it will match all values of a type [ty] for which
    [matches_type patt ty] returns [true]. *)
