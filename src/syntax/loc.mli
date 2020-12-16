(** {1 Location Tracking} *)

type pos = private {
  line: int; (** Line number *)
  col:  int; (** Column number *)
  off:  int  (** Offset into file *)
}
(** A position in a source file. *)

type t = private {
  fname:     string; (** Source File *)
  start_pos: pos;    (** Starting position *)
  end_pos:   pos;    (** Ending position *)
  length:    int     (** Length in characters *)
}
(** A location in a source file. *)

(** {2 Constructors} *)

val mock : string -> int * int * int -> int * int * int -> t
(** [mock fname start_pos end_pos] constructs a location in [fname] from
    [start_pos] to [end_pos].  The start and end positions are
    [(line, col, off)] triples.  Primarily used for testing. *)

val loc : Lexing.lexbuf -> t
(** [loc fname pos] constructs a location from the current lexeme recognized by
    a lexer. *)

exception MismatchedFileNames of string * string
(** Raised when two locations are from different files. *)

val span : t -> t -> t
(** [span start_loc end_loc] constructs a location that spans from [start_loc]
    to [end_loc].  Raises {!MismatchedFileNames} if [start_loc] and [end_loc]
    are from two different files. *)

(** {2 Operations} *)

val track : string -> Lexing.lexbuf -> unit
(** [track fname lexbuf] initializes [lexbuf] for location tracking with the
    filename set to [fname]. *)
