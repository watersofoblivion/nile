(** {1 Optimizer} *)

(** {2 Configuration} *)

type conf = private {
  tailcall   : bool;
  inline     : bool;
  ccp        : bool;
  max_passes : int;
}

val conf : bool -> bool -> bool -> int -> conf
(** [conf tailcall inline ccp max_passes] constructs a configuration. *)
