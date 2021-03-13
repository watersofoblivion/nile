(** {1 Optimizer} *)

(** {2 Configuration} *)

type conf = private {
  tailcall   : bool; (** Perform tail-call optimization *)
  inline     : bool; (** Perform inlining *)
  ccp        : bool; (** Perform conditional constant propagation *)
  max_passes : int;  (** Upper bound on optimizer iterations *)
}
(** Optimizer configuration *)

val conf : bool -> bool -> bool -> int -> conf
(** [conf tailcall inline ccp max_passes] constructs a configuration. *)
