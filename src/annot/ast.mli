open Format
open Common

(** {1 Annotated Abstract Syntax}
 *
 * The first intermediate form of for the compiler.
 *)

(** {2 Syntax} *)

type expr = private
  | Unit (** Unit *)
  | Bool of {
      b: bool (** Value *)
    } (** Boolean *)
  | Int of {
      i: int (** Value *)
    } (** Integer *)
  | Float of {
      f: float (** Value *)
    } (** Floating-point *)
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
      d: string
    } (** ISO-8601 Duration *)
  | Tuple of {
      exprs: expr list; (** Element Expressions *)
      ty:    Type.t     (** Type *)
    } (** Tuple *)
  | Record of {
      constr: Sym.sym option; (** Constructor *)
      fields: field list;     (** Element Expressions *)
      ty:     Type.t          (** Type *)
    } (** Record *)
  | Constr of {
      name: Sym.sym;   (** Constructor Name *)
      args: expr list; (** Arguments *)
      ty:   Type.t     (** Type *)
    } (** Variant Constructor *)
  | Var of {
      id: Sym.sym (** Identifier *)
    } (** Variable Identifier *)
  | UnOp of {
      op:   Op.un; (** Operator *)
      prec: int;   (** Precedence *)
      rhs:  expr   (** Operand *)
    } (** Unary Operator *)
  | BinOp of {
      op:   Op.bin; (** Operator *)
      prec: int;    (** Precedence *)
      lhs:  expr;   (** Left-Hand Operand *)
      rhs:  expr    (** Right-Hand Operand *)
    } (** Binary Operator *)
  | Slice of {
      expr:  expr;        (** Expression to Slice *)
      start: expr option; (** Begin Index *)
      stop:  expr option  (** End Index *)
    } (** Slice *)
  | Index of {
      expr: expr; (** Expression to Slice *)
      idx:  expr  (** Index *)
    } (** Index *)
  | Case of {
      scrut:   expr;        (** Scrutinee *)
      clauses: clause list; (** Match Clauses *)
      res:     Type.t       (** Result type *)
    } (** Case *)
  | Let of {
      binding: binding; (** Binding *)
      scope:   expr     (** Scope of Binding *)
    } (** Value Binding *)
  | LetRec of {
      bindings: binding list; (** Bindings *)
      scope: expr             (** Scope of Bindings *)
    } (** Recursive Value Bindings *)
  | TupleProj of {
      tuple: expr; (** Tuple *)
      index: int   (** Field Index *)
    } (** Tuple Projection *)
  | RecordProj of {
      record: expr;   (** Record *)
      field:  Sym.sym (** Field Name *)
    } (** Record Projection *)
  | Abs of {
      arity:  int;        (** Arity *)
      params: param list; (** Parameters *)
      res:    Type.t;     (** Result Type *)
      body:   expr        (** Body *)
    } (** Function Abstraction *)
  | App of {
      arity: int;      (** Arity *)
      fn:    expr;     (** Function *)
      args:  expr list (** Arguments *)
    } (** Function Application *)
(** An expression *)

and param = Param of {
  name: Patt.t; (** Name *)
  ty:   Type.t  (** Type *)
}
(** A function parameter *)

and binding = Binding of {
  name: Patt.t; (** Name *)
  ty:   Type.t; (** Type *)
  body: expr    (** Body *)
}
(** A value Binding *)

and clause = Clause of {
  patt: Patt.t; (** Pattern *)
  body: expr    (** Body *)
}
(** A pattern matching clause *)

type top = private
  | TopLet of {
      binding: binding (** Binding *)
    } (** Value binding *)
  | TopRec of {
      bindings: binding list (** Bindings *)
    } (** Recursive value bindings *)
(** A top-level statement *)

type pkg = Package of {
  name: Sym.sym; (** Name *)
  tops: top list (** Top-Level Statements *)
}
(** A source package *)

(** {2 Constructors} *)

(** {3 Expressions} *)

val unit : expr
(** [unit] constructs a unit literal. *)

val bool : bool -> expr
(** [bool b] constructs a boolean literal with the value [b]. *)

val int : int -> expr
(** [int i] constructs an integer literal with the value [i]. *)

val float : float -> expr
(** [float f] constructs a floating-point literal with the value [f]. *)

val rune : bytes -> expr
(** [string len s] constructs a string literal of length [len] with the value
    [s]. *)

val string : string -> expr
(** [string s] constructs a string literal  with the value [s]. *)

val blob : bytes -> expr
(** [blob bs] constructs a binary large object (BLOB) literal with the value
    [bs]. *)

val timestamp : string -> expr
(** [timestamp ts] constructs a ISO-8601 timestamp literal of length with the
    value [ts]. *)

val duration : string -> expr
(** [duration ts] constructs a ISO-8601 duration literal of length with the
    value [d]. *)

val tuple : expr list -> Type.t -> expr
(** [tuple exprs ty] constructs a tuple literal of type [ty] with the values
    [exprs]. *)

val record : Sym.sym option -> expr list -> Type.t -> expr
(** [record constr exprs ty] constructs a record literal of type [ty] with the
    constructor [constr] and the values [exprs]. *)

val constr : Sym.sym -> expr list -> Type.t -> expr
(** [constr name args ty] constructs a variant constructor literal of type [ty]
    with the constructor [name] and the arguments [args]. *)

val var : Sym.sym -> expr
(** [var sym] constructs a variable identifier expression referencing the value
    bound to the symbol [sym]. *)

val un_op : Op.un -> int -> expr -> expr
(** [un_op op prec rhs] constructs a unary operator expression applying [op] to
    [rhs] with precedence [prec]. *)

val bin_op : Op.bin -> int -> expr -> expr -> expr
(** [bin_op op prec lhs rhs] constructs a binary operator expression applying
    [op] to [lhs] and [rhs] with precedence [prec]. *)

val slice : expr -> expr option -> expr option -> expr
(** [slice expr start stop] constructs a slice operator slicing [expr] from
    [start] to [end]. *)

val index : expr -> expr -> expr
(** [index expr idx] constructs an index operator index [expr] at [idx]. *)

val case : expr -> clause list -> Type.t -> expr
(** [case scrut clauses res] constructs a case expression scrutinizing [scrut],
    branching to one of [clauses], and resulting in a value of type [res]. *)

val bind : binding -> expr -> expr
(** [bind b rest] constructs a value binding expression binding [b] within the
    scope of [rest]. *)

val bind_rec : binding list -> expr -> expr
(** [bind_rec bs rest] constructs a set of recursive value bindings over the
    list of bindings [bs] within the scope of both each other and [rest]. *)

val tuple_proj : expr -> int -> expr
(** [tuple_proj tuple index] constructs a projection of the [index]'th field of
    the tuple [tuple]. *)

val record_proj : expr -> Sym.sym -> expr
(** [record_proj record field] constructs a projection of the field named
    [field] of the record [record]. *)

val abs : int -> param list -> Type.t -> expr -> expr
(** [abs arity params res expr] constructs a function abstraction expression of
    arity [arity] binding the parameters within the scope of the function body
    [expr] with the result type [res]. *)

val app : int -> expr -> expr list -> expr
(** [app arity f xs] constructs a function application expression applying the function
    [f] to the values [xs]. *)

val binding : Patt.t -> Type.t -> expr -> binding
(** [binding patt ty expr] constructs a value binding expression binding the
    value [expr] of type [ty] to pattern [patt]. *)

val clause : Patt.t -> expr -> clause
(** [clause patt expr] constructs a pattern matching clause that executes [expr]
    when [patt] is matched. *)

(** {3 Top-Level Statements} *)

val top_bind : binding -> top
(** [top_bind b] constructs a top-level value binding expression binding [b]. *)

val top_bind_rec : binding list -> top
(** [top_bind_rec bs rest] constructs a set of recursive top-level value
    bindings over the list of bindings [bs]. *)

(** {3 Packages} *)

val pkg : Sym.sym -> top list -> pkg
(** [pkg name tops] constructs a source package named [name] consisting of the
    list of top-level bindings [tops]. *)

(** {2 Annotation} *)

val annotate_expr : Type.env -> Ast.expr -> expr
(** [annotate_expr env expr] type-checks the abstract syntax expression [expr]
    in the type environment [env] and returns an annotated syntax expression. *)

val annotate_top : Type.env -> Ast.top -> (Type.env * top)
(** [annotate_top env top] type-checks the abstract syntax top-level statement
    [top] in the type environment [env] and returns an annotated syntax
    top-level statement along with a type environment with the top-level
    statement bound. *)

val annotate : Type.env -> Ast.file list -> (Type.env * pkg)
(** [annotate env files] type-checks the collection of abstract syntax files
    [files] as a single package in the type environment [env] and returns an
    annotated syntax package along with a type environment with all of the top
    level statements in the package bound. *)

(** {2 Operations} *)

val precedence : expr -> int
(** [precedence expr] returns the precedence of the express [expr]. *)
