open Common

(** {1 Types} *)

type t = private
  | Unit of Loc.t                            (** Unit *)
  | Bool of Loc.t                            (** Booleans *)
  | Int of Loc.t                             (** Integers *)
  | Float of Loc.t                           (** Floating-point *)
  | String of Loc.t                          (** Strings *)
  | Blob of Loc.t                            (** Binary Large Object (BLOB) *)
  | Timestamp of Loc.t                       (** ISO-8601 Timestamp *)
  | Duration of Loc.t                        (** ISO-8601 Duration *)
  | Fun of Loc.t * t list * t                (** Functions *)
  | Tuple of Loc.t * int * t list            (** Tuple *)
  | Record of Loc.t * field list             (** Record *)
  | Variant of Loc.t * constr list           (** Variant *)
  | Package of Loc.t * t Sym.map * t Sym.map (** Package *)
(** Types *)

and field = Loc.t * Sym.sym * t
(** Record field *)

and constr = Loc.t * Sym.sym * t option
(** Variant constructor *)

(** {2 Constructors} *)

val unit : Loc.t -> t
(** [unit loc] constructs a unit type at location [loc]. *)

val bool : Loc.t -> t
(** [bool loc] constructs a boolean type at location [loc]. *)

val int : Loc.t -> t
(** [int loc] constructs an integer type at location [loc]. *)

val float : Loc.t -> t
(** [float loc] constructs a floating-point type at location [loc]. *)

val string : Loc.t -> t
(** [string loc] constructs a string type at location [loc]. *)

val blob : Loc.t -> t
(** [blob loc] constructs a binary large object (BLOB) type at location [loc].
    *)

val timestamp : Loc.t -> t
(** [timestamp loc] constructs a ISO-8601 timestamp type at location [loc]. *)

val duration : Loc.t -> t
(** [duration loc] constructs a ISO-8601 duration type at location [loc]. *)

val func : Loc.t -> t list -> t -> t
(** [func loc args ret] constructs a function type at location [loc] mapping
    argument values of types [args] to values of type [ret]. *)

val tuple : Loc.t -> t list -> t
(** [tuple loc tys] constructs a tuple type at location [loc] with element types
    [tys]. *)

val record : Loc.t -> field list -> t
(** [record loc fields] constructs a record type at location [loc] with field
    [fields]. *)

val variant : Loc.t -> constr list -> t
(** [variant loc constrs] constructs a variant type at location [loc] with
    constructors [constrs]. *)

val pkg : Loc.t -> t Sym.map -> t Sym.map -> t
(** [pkg loc tys fns] constructs a package type at location [loc] containing
    types [tys] and functions [fns]. *)

val field : Loc.t -> Sym.sym -> t -> field
(** [field loc id ty] constructs a record field at location [loc] named [id] of
    type [ty]. *)

val constr : Loc.t -> Sym.sym -> t option -> field
(** [constr loc id ty] constructs a variant constructor at location [loc] named
    [id] with optional type [ty]. *)

(** {2 Operations} *)

val loc : t -> Loc.t
(** [loc ty] returns the location of the type [ty]. *)

val equal : t -> t -> bool
(** [equal x y] compares two types for equality. *)

(** {2 Type Checking} *)

(** {3 Environments} *)

type env
(** A type environment *)

val env : env
(** [env] constructs an empty type environment. *)

val bind : Patt.t -> t -> env -> env
(** [bind patt ty env] constructs copy of [env] extended by binding the type
    [ty] to the pattern [patt].  Any type previously bound to [patt] is masked.
    The original environment is left unaltered. *)

val lookup : Sym.sym -> env -> t
(** [lookup sym env] finds the type bound to the symbol [sym] in the environment
    [env], or raises {!Not_found} if the symbol is unbound. *)

(** {3 Patterns} *)

val of_pattern : Patt.t -> t -> bool
(** [of_pattern patt ty] tests whether or not the pattern [patt] matches
    values of type [ty]. *)

(** {3 Errors} *)

exception DeclarationMismatch of Patt.t * t * t
(** Raised when the declared type of a let-bound value does not match its actual
    type.  Contains the identifier, the expected type, and the actual type. *)

exception ResultMismatch of t * t
(** Raised when the declared result type of a let-bound function does not match
    the type of the function body.  Contains the expected and actual types. *)

exception UnboundIdentifier of Sym.sym
(** Raised when an identifier has not been bound and therefore has no type.
    Contains the identifier that was unbound. *)

exception CannotApply of t
(** Raised when a value that is not a function is applied to an argument.
    Contains the type of the value that was applied. *)

exception TooManyArgs of t * int
(** Raised when a function is applied to too many arguments.  Contains the type
    of the function and the number of arguments it was applied to. *)

exception InvalidArgs of t * t
(** Raised when a function is applied to an argument of invalid type.  Contains
    the expected and the actual argument types. *)

exception InvalidCondition of t
(** Raised when the condition clause of a conditional statement is not a boolean
    typed value.  Contains the actual type of the condition clause. *)

exception ConditionalBranchMismatch of t * t
(** Raised when the true and false branches of a conditional statement do not
    have the same type.  Contains the types of the true and false branches. *)

exception AnnotationRequired of Patt.t
(** Raised when a required annotation is not present.  Contains the
    identifier that requires an annotation. *)

val declaration_mismatch : Patt.t -> t -> t -> 'a
(** [declaration_mismatch sym expected actual] raises a {!DeclarationMismatch}
    exception. *)

val result_mismatch : t -> t -> 'a
(** [result_mismatch expected actual] raises a {!ResultMismatch} exception. *)

val unbound_identifier : Sym.sym -> 'a
(** [unbound_identifier sym] raises an {!UnboundIdentifier} exception. *)

val cannot_apply : t -> 'a
(** [cannot_apply ty] raises a {!CannotApply} exception. *)

val invalid_args : t -> t -> 'a
(** [invalid_args expected actual] raises an {!InvalidArgs} exception. *)

val invalid_condition : t -> 'a
(** [invalid_condition ty] raises an {!InvalidCondition} exception. *)

val conditional_branch_mismatch : t -> t -> 'a
(** [conditional_branch_mismatch t f] raises an {!ConditionalBranchMismatch}
    exception. *)

val annotation_required : Patt.t -> 'a
(** [annotation_required sym] raises an {!AnnotationRequired} exception. *)
