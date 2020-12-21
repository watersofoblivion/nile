(* open Format *)
open Common

(** {1 Administrative Normal Form Intermediate Representation} *)

type t =
  | Bool of bool                             (** Boolean *)
  | Int of int                               (** Integer *)
  | Var of string                            (** Variable *)
  | UnOp of Op.un * t                        (** Unary Operator *)
  | BinOp of t * Op.bin * t                  (** Binary Operator *)
  | If of t * t * t                          (** Conditional *)
  | Let of string * Type.t * t * t           (** Value Binding *)
  | LetRec of (string * Type.t * t) list * t (** Recursive Value Binding *)
  | Abs of string * t                        (** Function Abstraction *)
  | App of t * t                             (** Function Application *)
(** Administrative Normal Form Intermediate Representation *)
