open Cmdliner

(** {1 Command-Line Interface} *)

module Args :
  sig
    module OptArgs :
      sig
        val tailcall : bool Term.t
        (** [tailcall] enables tail-call optimization. *)

        val inline : bool Term.t
        (** [inline] enables the inliner. *)

        val ccp : bool Term.t
        (** [ccp] enables conditional constant propagation. *)

        val max_passes : int Term.t
        (** [max_passes] sets the upper bound on the number of optimization
            passes. *)

        val term : Opt.conf Term.t
        (** [term] builds an optimizer configuration from the command-line
            arguments. *)
      end
    (** Optimizer arguments *)

    module ClosArgs :
      sig
        val mode : Clos.mode Term.t
        (** [mode] sets the closure conversion mode. *)

        val term : Clos.conf Term.t
        (** [term] builds a closure conversion configuration from the
            command-line arguments. *)
      end
    (** Closure conversion arguments *)

    module DumpArgs :
      sig
        type conf = private {
          unannot_ast : bool; (** Un-annotated AST *)
          annot_ast   : bool; (** Annotated AST *)
          unopt_ir    : bool; (** Unoptimized Intermediate Representation (ANF) *)
          opt_ir      : bool; (** Optimized Intermediate Representation (ANF) *)
          clos        : bool; (** Closure converted program *)
          unopt_llvm  : bool; (** Unoptimized LLVM assembly *)
          opt_llvm    : bool; (** Optimized LLVM assembly *)
        }
        (** Dump options *)

        val conf : bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> conf
        (** [conf unannot_ast annot_ast unopt_ir opt_ir clos unopt_llvm opt_llvm all]
            constructs a dump configuration. *)

        val unannot_ast : bool Term.t
        (** [unannot_ast] dumps the un-annotated abstract syntax tree to STDERR.
            *)

        val annot_ast : bool Term.t
        (** [annot_ast] dumps the annotated abstract syntax tree to STDERR. *)

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
          opt  : Opt.conf; (** Optimizer configuration *)
          clos : Clos.conf; (** Closure conversion configuration *)
          dump : DumpArgs.conf; (** Dump configuration *)
        }
        (** Compiler configuration *)

        val conf : Opt.conf -> Clos.conf -> DumpArgs.conf -> conf
        (** [conf opt clos dump] constructs a compiler configuration. *)

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
