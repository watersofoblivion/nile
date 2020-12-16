open OUnit2
open Nile

(** {1 Closure Conversion} *)

val suite : test
(** [suite] is the test suite for closure conversion. *)

(** {2 Assertions} *)

val assert_conf : ctxt:test_ctxt -> Clos.mode -> Clos.conf -> unit
(** [assert_conf ~ctxt mode conf] asserts the contents of the closure conversion
    configuration [conf]. *)
