open OUnit2
open Nile.Common

(** {1 Types} *)

val suite : test
(** [suite] is the test suite for types. *)

(** {2 Assertions} *)

val assert_type_equal : ctxt:test_ctxt -> Type.t -> Type.t -> unit
(** [assert_type_equal ~ctxt expected actual] asserts that the type [actual] is
    equal to the type [expected]. *)
