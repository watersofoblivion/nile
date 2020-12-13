open OUnit2
open Nile

(** {1 Operators} *)

val suite : test
(** [suite] is the test suite for operators. *)

(** {2 Assertions} *)

val assert_un_equal : ctxt:test_ctxt -> Op.un -> Op.un -> unit
(** [assert_un_equal ~ctxt expected actual] asserts that the unary operator
    [actual] is equal to the unary operator [expected]. *)

val assert_bin_equal : ctxt:test_ctxt -> Op.bin -> Op.bin -> unit
(** [assert_bin_equal ~ctxt expected actual] asserts that the binary operator
    [actual] is equal to the binary operator [expected]. *)
