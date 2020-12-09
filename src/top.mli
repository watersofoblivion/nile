open Format

(** {1 Toplevel Statements} *)

type 'a t =
  | Let of 'a         (** Value binding *)
  | LetRec of 'a list (** Recursive value binding *)

(** {2 Operations} *)

val pp: ('a -> formatter -> unit) -> 'a t -> formatter -> unit
(** [pp ptr top fmt] pretty-prints [top] to the formatter [top], using [ptr] to
    pretty-print the bound values. *)
