open Cmdliner

(** {1 Command-Line Interface} *)

module Args :
  sig
    module DumpArgs :
      sig
        type conf = private {
          ast:        bool; (** Abstract Syntax Tree *)
          unopt_ir:   bool; (** Unoptimized Intermediate Representation (ANF) *)
          opt_ir:     bool; (** Optimized Intermediate Representation (ANF) *)
          clos:       bool; (** Closure converted program *)
          unopt_llvm: bool; (** Unoptimized LLVM assembly *)
          opt_llvm:   bool; (** Optimized LLVM assembly *)
        }
        (** Dump options *)

        val conf : bool -> bool -> bool -> bool -> bool -> bool -> bool -> conf
        (** [conf ast unopt_ir opt_ir clos unopt_llvm opt_llvm all]
            constructs a dump configuration. *)

        val ast : bool Term.t
        (** [ast] dumps the abstract syntax tree to STDERR.
            *)

        val unopt_ir : bool Term.t
        (** [unopt_ir] dumps the unoptimized intermediate representation (ANF)
            to STDERR. *)

        val opt_ir : bool Term.t
        (** [opt_ir] dumps the optimized intermediate representation (ANF) to
            STDERR. *)

        val clos : bool Term.t
        (** [clos] dumps the closure convered program to STDERR. *)

        val unopt_llvm : bool Term.t
        (** [unopt_llvm] dumps the unoptimized LLVM assembly to STDERR. *)

        val opt_llvm : bool Term.t
        (** [opt_llvm] dumps the optimized LLVM assembly to STDERR. *)

        val all : bool Term.t
        (** [all] dumps all intermediate representations to STDERR. *)

        val term : conf Term.t
        (** [term] constructs a dump configuration from the command-line
            arguments. *)
      end
    (** Internal representations arguments *)

    module CompilerArgs :
      sig
        type conf = private {
          dump : DumpArgs.conf; (** Dump configuration *)
        }
        (** Compiler configuration *)

        val conf : DumpArgs.conf -> conf
        (** [conf dump] constructs a compiler configuration. *)

        val term : conf Term.t
        (** [term] constructs a compiler configuration from the command-line
            arguments. *)
      end
    (** Compiler configuration *)
  end
(** Shared command-line arguments *)

module Cmds :
  sig
    module DefaultCmd :
      sig
        val cmd : string Term.ret Term.t * Term.info
        (** [cmd] is the default command, run when no subcommand is given.  It
            simply displays the help. *)
      end
    (** The default command, run when no commands are given *)

    module BuildCmd :
      sig
        val src : string Term.t
        (** [src] is the source file to compile. *)

        val exe : string option Term.t
        (** [exe] is the target executable to generate.  By default, this is the
            source executable with its extension replaced by ".exe". *)

        val compile : string -> string option -> Args.CompilerArgs.conf -> unit

        val cmd : unit Term.t * Term.info
      end
    (** The command to build an executable from a source file. *)
  end
(** Top-level commands *)

val main : string array -> unit
(** [main argv] processes the command-line arguments [argv]. *)
