open OUnit2
open Nile.Ir

(** {1 Optimizer} *)

val suite : test
(** [suite] is the test suite for the optimizer. *)

(** {2 Assertions} *)

val assert_conf : ctxt:test_ctxt -> bool -> bool -> bool -> int -> Opt.conf -> unit
(** [assert_conf ~ctxt tailcall inliner ccp max_passes actual] asserts that the
    optimizer configuration [actual] has various flags set. *)
