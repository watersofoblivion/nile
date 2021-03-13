open Common

(** {1 Operators} *)

type t = private {
  loc:        Loc.t; (** Location Tracking Information *)
  precedence: int;   (** Precedence *)
  lexeme:     string (** Lexeme *)
}
(** Operator *)

(** {3 Constructors} *)

val op : Loc.t -> int -> string -> op
(** [op loc precedence lexeme] constructs an operator at location [loc] with
    precedence [precedence] and lexeme [lexeme]. *)

(** {3 Operations} *)

val loc : un -> Loc.t
(** [un_loc op] returns the locatio of the unary operator [op]. *)

val precedence : un -> int
(** [un_precedence op] returns the operator precedence of the unary operator
    [op]. *)
