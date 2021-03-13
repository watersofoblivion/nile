open Format
open Common

(** {1 Administrative Normal Form Intermediate Representation} *)

(** {2 Syntax} *)

type atom = private
  | Unit (** Unit *)
  | Bool of {
      b: bool (** Value *)
    } (** Boolean *)
  | Int of {
      i: int (** Value *)
    } (** Integer *)
  | Float of {
      f: float (** Value *)
    } (** Floating Point *)
  | Rune of {
      r: bytes (** Value *)
    } (** Rune *)
  | String of {
      length: int;   (** Length *)
      s:      string (** Value *)
    } (** String *)
  | Byte of {
      b: bytes (** Value *)
    } (** Byte *)
  | Blob of {
      length: int;  (** Length *)
      bs:     bytes (** Value *)
    } (** Binary Large Object (BLOB) *)
  | Timestamp of {
      ts: string (** Value *)
    } (** ISO-8601 Timestamp *)
  | Duration of {
      d: string (** Value *)
    } (** ISO-8601 Duration *)
  | Var of {
      id: Sym.sym (** Name *)
    } (** Variable *)
  | Abs of {
      arity:  int;        (** Arity *)
      join:   bool;       (** Join Point *)
      params: param list; (** Parameters *)
      res:    Type.t;     (** Result Type *)
      body:   block       (** Body *)
    } (** Function Abstraction *)
(** Atomic Values *)

and param = Param of {
  name: Patt.t; (** Name *)
  ty:   Type.t  (** Type *)
}
(** Function Parameter *)

and expr = private
  | App of {
      arity: int;      (** Arity *)
      tail:  bool;     (** Tail-call *)
      jump:  bool;     (** Jump *)
      fn:    atom;     (** Function *)
      args:  atom list (** Arguments *)
    } (** Function Application *)
  | Builtin of {
      builtin: Builtin.t; (** Builtin Function *)
      args:    atom list  (** Arguments *)
    } (** Builtin Function Application *)
  | Tuple of {
      arity: arity;    (** Arity *)
      exprs: atom list (** Element Expressions *)
    } (** Tuple construction *)
  | Proj of {
      tuple: atom; (** Tuple *)
      index: int   (** Index *)
    } (** Tuple projection *)
  | Constr of {
      name: Sym.sym;   (** Constructor Name *)
      args: atom list; (** Arguments *)
      ty:   Type.t     (** Constructor Type *)
    } (** Constructor construction *)
  | Atom of {
      a: atom (** Atomic Value *)
    } (** Atomic Value *)
(** Primitive Expressions *)

and block = private
  | Let of binding * block              (** Value Binding *)
  | LetRec of binding list * block      (** Recursive Value Bindings *)
  | Case of atom * clause list * Type.t (** Case *)
  | Expr of expr                        (** Primitive Expression *)
(** Block Expressions *)

and binding = Patt.t * Type.t * expr
(** Bound Value *)

and clause = Patt.t * block
(** Match Clause *)

type top = private
  | TopLet of binding      (** Value binding *)
  | TopRec of binding list (** Recursive value bindings *)
(** A top-level statement *)

type file = top list
(** A source file *)

(** {2 Constructors} *)

(** {3 Atomic Values} *)

val unit : atom
(** [unit] constructs an atomic unit literal. *)

val bool : bool -> atom
(** [bool b] constructs an atomic boolean literal with the value [b]. *)

val int : int -> atom
(** [int i] constructs an atomic integer literal with the value [i]. *)

val float : float -> atom
(** [float f] constructs an atomic floating-point literal with the value [f]. *)

val string : string -> atom
(** [string s] constructs an atomic string literal with the value [s]. *)

val byte : bytes -> atom
(** [byte b] constructs an atomic byte literal with the value [b]. *)

val blob : bytes -> atom
(** [blob bs] constructs an atomic binary large object (BLOB) literal with the
    value [bs]. *)

val timestamp : string -> atom
(** [timestamp ts] constructs an atomic ISO-8601 timestamp literal with the
    value [ts]. *)

val duration : string -> atom
(** [duration d] constructs an atomic ISO-8601 duration literal with the value
    [d]. *)

val var : Sym.sym -> atom
(** [var sym] constructs an atomic variable referencing the bound value [sym]. *)

val abs : param list -> Type.t -> block -> atom
(** [abs params res body] constructs an atomic function abstraction binding the
    parameters [params] within [body] and resulting in values of type [res]. *)

val join : param list -> Type.t -> block -> atom
(** [join params res body] constructs an atomic join point binding the
    parameters [params] within [body] and resulting in values of type [res]. *)

val param : Patt.t -> Type.t -> param
(** [param patt ty] constructs a function parameter binding values matching the
    pattern [patt] of type [ty]. *)

(** {3 Primitive Expressions} *)

val app : atom -> atom list -> expr
(** [app f xs] constructs a function application expression applying the
    function [f] to the atomic argument values [xs]. *)

val tail : atom -> atom list -> expr
(** [tail f xs] constructs a tail-position function application expression
    applying the function [f] to the atomic argument values [xs]. *)

val jump : atom -> atom list -> expr
(** [jump j xs] constructs a join point application expression applying the join
    point [j] to the atomic argument values [xs]. *)

val builtin : Sym.sym -> atom list -> expr
(** [builtin b xs] constructs a builtin function application expression applying
    the builtin [b] to the atomic argument values [xs]. *)

val tuple : atom list -> expr
(** [tuple xs] constructs a tuple construction expression packaging the atomic
    values [xs] into a tuple. *)

val proj : atom -> int -> expr
(** [proj tuple field] constructs a tuple projection expression selecting the
    field [field] from the tuple [tuple].  Fields are 1-indexed, not zero. *)

val constr : Sym.sym -> atom option -> expr
(** [constr id value] constructs a variant construction construction expression
    constructing the construction [id] with the atomic value [value]. *)

val atom : atom -> expr
(** [atom a] constructs a primitive expression representing the atomic value
    [a]. *)

(** {3 Block Expressions} *)

val bind : binding -> block -> block
(** [bind b rest] constructs a variable binding expression binding [b] within
    [rest]. *)

val bind_rec : binding list -> block -> block
(** [bind_rec bs rest] constructs a recursive variable binding expression
    binding all of [bs] within each other and within [rest]. *)

val case : atom -> clause list -> Type.t -> block
(** [case scrut clauses res] constructs a case expression branching depending on
    the value of the atomic expression [scrut] and resulting in a value of type
    [res]. *)

val expr : expr -> block
(** [expr e] constructs a block expression representing the primitive expression
    [e]. *)

(** {3 Variable Bindings} *)

val binding : Patt.t -> Type.t -> expr -> binding
(** [binding patt ty expr] constructs a variable binding which binds the pattern
    [patt] of type [ty] to the value of [expr]. *)

(** {3 Match Clauses} *)

val clause : Patt.t -> block -> clause
(** [clause patt body] constructs a pattern matching clause which binds the
    pattern [patt] in [body]. *)

(** {3 Top-Level Statements} *)

val top_bind : binding -> top
(** [top_bind b] constructs a value binding expression binding [b]. *)

val top_bind_rec : binding list -> top
(** [top_bind_rec bs rest] constructs a set of recursive top-level value
    bindings over the list of bindings [bs]. *)

(** {3 Files} *)

val file : top list -> file
(** [file tops] constructs a source file consisting of the list of top-level
    bindings [tops]. *)
