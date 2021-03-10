open Common

(** {1 Abstract Syntax}
 *
 * The external syntax of the language.
 *)

(** {2 Syntax}
 *
 * Data types representing all of the various constructs in the external syntax.
 *)

(** {3 Expressions} *)

type expr = private
  | Unit of Loc.t                                    (** Unit *)
  | Bool of Loc.t * bool                             (** Boolean *)
  | Int of Loc.t * int                               (** Integer *)
  | Float of Loc.t * float                           (** Floating-point *)
  | String of Loc.t * int * string                   (** String *)
  | Blob of Loc.t * int * bytes                      (** Binary Large Object (BLOB) *)
  | Timestamp of Loc.t * string                      (** ISO-8601 Timestamp *)
  | Duration of Loc.t * string                       (** ISO-8601 Duration *)
  | Tuple of Loc.t * expr list                       (** Tuples *)
  | Record of Loc.t * Sym.sym option * field list    (** Record *)
  | Var of Loc.t * Sym.sym                           (** Variable Identifier *)
  | UnOp of Loc.t * Op.un * expr                     (** Unary Operation *)
  | BinOp of Loc.t * expr * Op.bin * expr            (** Binary Operation *)
  | If of Loc.t * expr * expr * expr                 (** Conditional *)
  | Case of Loc.t * expr * clause list               (** Case *)
  | Let of Loc.t * binding * expr                    (** Value Binding *)
  | LetRec of Loc.t * binding list * expr            (** Recursive Value Bindings *)
  | Abs of Loc.t * param list * Type.t option * expr (** Function Abstraction *)
  | App of Loc.t * expr * expr list                  (** Function Application *)
(** An expression *)

and field = Loc.t * Sym.sym * expr
(** A record field *)

and param = Loc.t * Patt.t * Type.t
(** A parameter *)

and binding = Loc.t * Patt.t * Type.t option * expr
(** A value Binding *)

and clause = Loc.t * Patt.t * expr
(** A pattern matching clause *)

(** {3 Top-Level Statements} *)

type top = private
  | Val of Loc.t * binding           (** Value binding *)
  | Def of Loc.t * binding           (** Function binding *)
  | Type of Loc.t * Sym.sym * Type.t (** Type binding *)
(** A top-level statement *)

(** {3 Import Statements} *)

type name = Loc.t * string
(** Import name *)

type version = Loc.t * int
(** Major version number *)

type from = Loc.t * (name * version) option
(** From clause.  If the name/version pair is given, it specifies an external
    package to import.  Otherwise, it specifies the current package. *)

type alias = Loc.t * name * name option
(** Package name and optional local alias.  If no local alias is given, the
    module's declared name is used. *)

type pkgs = Loc.t * alias list
(** List of packages to import from a module. *)

type import = Loc.t * from option * pkgs
(** Import statement *)

(** {3 Package Statements} *)

type pkg = Loc.t * name
(** Package statement declaring the name of the package this file is in. *)

(** {3 Source Files} *)

type file = pkg * import list * top list
(** A source file *)

(** {2 Constructors}
 *
 * Functions to construct Abstract Syntax Trees.
 *)

(** {3 Expressions} *)

val unit : Loc.t -> expr
(** [unit loc] constructs a unit literal at location [loc]. *)

val bool : Loc.t -> bool -> expr
(** [bool loc b] constructs a boolean literal with the value [b] at location
    [loc]. *)

val int : Loc.t -> int -> expr
(** [int loc i] constructs an integer literal with the value [i] at location
    [loc]. *)

val float : Loc.t -> float -> expr
(** [float loc f] constructs a floating-point literal with the value [f] at
    location [loc]. *)

val string : Loc.t -> int -> string -> expr
(** [string loc len s] constructs a string literal with the value [s] of length
    [len] at location [loc]. *)

val blob : Loc.t -> int -> bytes -> expr
(** [blob loc len bs] constructs a binary large object (BLOB) literal with the
    value [bs] of length [len] at location [loc]. *)

val timestamp : Loc.t -> string -> expr
(** [timestamp loc ts] constructs a ISO-8601 timestamp literal with the value
    [ts] at location [loc]. *)

val duration : Loc.t -> string -> expr
(** [duration loc d] constructs a ISO-8601 duration literal with the value [d]
    at location [loc]. *)

val tuple : Loc.t -> expr list -> expr
(** [tuple loc exprs] constructs a tuple literal with the values [exprs] at
    location [loc].  The location should span from the opening parenthesis to
    the closing parenthesis. *)

val record : Loc.t -> Sym.sym option -> field list -> expr
(** [record loc constr fields] constructs a record literal with the constructor
    [constr] and the fields [fields] at location [loc].  The location should
    span from the beginning of the constructor to the closing brace. *)

val var : Loc.t -> Sym.sym -> expr
(** [var loc sym] constructs a variable identifier expression at location [loc]
    referencing the value bound to the symbol [sym]. *)

val un_op : Loc.t -> Op.un -> expr -> expr
(** [un_op loc op r] constructs a unary operator expression at location [loc]
    with the operator [op] operating on [r].  The location should span from [op]
    to [r]. *)

val bin_op : Loc.t -> expr -> Op.bin -> expr -> expr
(** [bin_op loc l op r] constructs a binary operator expression at location
    [loc] with the operator [op] operating on [l] and [r].  The location should
    span from [l] to [r]. *)

val cond : Loc.t -> expr -> expr -> expr -> expr
(** [cond loc c t f] constructs a conditional expression at location [loc]
    branching on the value of the expression [c].  If [c] is true, then the
    expression [t] is evaluated.  Otherwise, the expression [f] is evaluated.
    The location should span from the initial "if" keyword to the end of [f]. *)

val case : Loc.t -> expr -> clause list -> expr
(** [case loc scrut clauses] constructs a case expression at location [loc]
    scrutinizing [scrut] and branching to one of [clauses].  The location should
    span from the initial "case" keyword to the end of the last clause. *)

val bind : Loc.t -> binding -> expr -> expr
(** [bind loc b rest] constructs a value binding expression at location [loc]
    binding [b] within the scope of [rest].  The location should span from the
    initial "let" keyword to the end of the [rest] expression. *)

val bind_rec : Loc.t -> binding list -> expr -> expr
(** [bind_rec loc bs rest] constructs a set of recursive value bindings at the
    location [loc] over the list of bindings [bs] within the scope of both each
    other and [rest].  The location should span from the initial "let" keyword
    to the end of the [rest] expression. *)

val abs : Loc.t -> param list -> Type.t option -> expr -> expr
(** [abs loc params res expr] constructs a function abstraction expression at
    location [loc] binding the parameter [params] within the scope of the
    function body [expr] with the result type [res].  The location should span
    from the first element of [params] to the end of [body].  If not provided,
    the type [res] will be inferred. *)

val app : Loc.t -> expr -> expr list -> expr
(** [app loc f xs] constructs a function application expression at location
    [loc] applying the function [f] to the values [xs].  The location should
    span from the beginning of [f] to the end of the last element of [xs]. *)

val field : Loc.t -> Sym.sym -> expr -> field
(** [field loc name expr] constructs a record field at location [loc] binding
    the field [name] to the value [expr].  The location should span from the
    beginning of [name] to the end of [expr]. *)

val param : Loc.t -> Patt.t -> Type.t -> param
(** [param loc patt ty] constructs a function parameter at location [loc]
    binding arguments matching the [patt] of type [ty].  The location should
    span from the beginning of [patt] to the end of [ty]. *)

val binding : Loc.t -> Patt.t -> Type.t option -> expr -> binding
(** [binding loc patt ty expr] constructs a value binding expression at location
    [loc] binding the value [expr] of type [ty] to pattern [patt].  The location
    should span from the beginning of [patt] to the end of [expr].  If not
    provided, the type [ty] will be inferred. *)

val clause : Loc.t -> Patt.t -> expr -> clause
(** [clause loc patt expr] constructs a pattern matching clause at location
    [loc] that executes [expr] when [patt] is matched.  The location should span
    from the beginning of [patt] to the end of [expr]. *)

(** {3 Top-Level Statements} *)

val top_val : Loc.t -> binding -> top
(** [top_val loc b] constructs a top-level value binding expression at location
    [loc] binding [b].  The location should span from the initial "val" keyword
    to [b]. *)

val top_def : Loc.t -> binding -> top
(** [top_def loc b] constructs a top-level function binding expression at
    location [loc] binding [b].  The location should span from the initial "def"
    keyword to [b]. *)

val top_type : Loc.t -> Sym.sym -> Type.t -> top
(** [top_type loc id ty] constructs a top-level type definition at location
    [loc] binding the type [ty] to the name [id].  The location should span from
    the initial "type" keyword to the end of [ty]. *)

(** {3 Import Statements} *)

val name : Loc.t -> string -> name
(** [name loc id] constructs a name at location [loc] with the name [id]. *)

val version : Loc.t -> int -> version
(** [version loc v] constructs a major version number at location [loc] with
    version [v]. *)

val from : Loc.t -> (name * version) option -> from
(** [from loc src] constucts a from clause at location [loc] importing from the
    source module [src].  If [src] is given, it specifies an external package to
    import from.  Otherwise, it specifies the current package. *)

val alias : Loc.t -> name -> name option -> alias
(** [alias name local] constructs an alias clause at location [loc] with package
    name [name] and an optional alias [local].  If no local alias is given, the
    module's declared name is used. *)

val pkgs : Loc.t -> alias list -> pkgs
(** [pkgs loc aliases] constructs a set of module imports at location [loc]
    importing packages [aliases]. *)

val import : Loc.t -> from option -> pkgs -> import
(** [import loc from pkgs] constructs an import statement at location [loc]
    importing [pkgs] from the module [from]. *)

(** {3 Package Statements} *)

val pkg : Loc.t -> name -> pkg
(** [pkg loc name] constructs a package statement at location [loc] declaring
    the name of the package to be [name]. *)

(** {3 Source Files} *)

val file : pkg -> import list -> top list -> file
(** [file pkg imports tops] constructs a source file in the package [pkg],
    importing the packages [imports] containing the list of top-level bindings
    [tops]. *)

(** {2 Operations}
 *
 * Common operations on Abstract Syntax Trees.
 *)

val precedence : expr -> int
(** [precedence expr] returns the precedence of the express [expr]. *)

val loc_expr : expr -> Loc.t
(** [loc_expr expr] returns the location information associated with the
    expression [expr]. *)

val loc_top : top -> Loc.t
(** [loc_top expr] returns the location information associated with the
    top-level binding [top]. *)
