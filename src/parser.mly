%{
  open Format

(*  let make_bind = Ast.bind *)
(*  let make_bind_rec = Ast.bind_rec *)
(*  let make_cond = Ast.cond *)

  let make_binding (loc, id) ty expr = Ast.binding ~loc id ty expr

  let make_un_op op_loc op r =
    let loc = Loc.span op_loc (Ast.loc r) in
    Ast.un_op ~loc op r
  let make_bin_op l op r =
    let loc = Loc.span (Ast.loc l) (Ast.loc r) in
    Ast.bin_op ~loc l op r

  let make_bool (loc, b) = Ast.bool ~loc b
  let make_int (loc, i) = Ast.int ~loc i
  let make_var (loc, id) = Ast.var ~loc id

  let make_prim_ty (_, id) = match id with
    | "Int" -> Type.int
    | "Bool" -> Type.bool
    | ty -> failwith (sprintf "Unknown type %S" ty)
  let make_fun_ty a b = Type.func a b
%}

%token <Loc.t> LPAREN RPAREN COLON ARROW BIND COMMA
%token <Loc.t> LET REC AND IN
%token <Loc.t> IF THEN ELSE
%token <Loc.t> ADD SUB MUL DIV MOD
%token <Loc.t> LAND LOR LNOT
%token <Loc.t> EQ NEQ
%token <Loc.t> LTE LT GT GTE
%token <Loc.t> EOF
%token <Loc.t * int> INT
%token <Loc.t * bool> BOOL
%token <Loc.t * string> LIDENT UIDENT

/*
%left  LET BIND IN
%left  IF
%left  THEN
%left  ELSE
*/
%left  LAND LOR
%right LNOT
%left  EQ NEQ
%left  LTE LT GT GTE
%left  ADD SUB
%left  MUL DIV MOD
%right ARROW

%type <Ast.b Top.t list> top

%start top

%%

top:
  LET binding top      { (Top.Let $2) :: $3 }
| LET REC bindings top { (Top.LetRec $3) :: $4 }
| EOF                  { [] }
;

bindings:
  binding AND bindings { $1 :: $3 }
| binding              { [$1] }
;

binding:
  LIDENT COLON ty BIND abs { make_binding $1 $3 $5 }
;

ty:
  UIDENT           { make_prim_ty $1 }
| ty ARROW ty      { make_fun_ty $1 $3 }
| LPAREN ty RPAREN { $2 }
;

abs:
  expr { $1 }
;

expr:
/*  LET binding IN expr         { make_bind $2 $4 } */
/*| LET REC bindings IN expr    { make_bind_rec $3 $5 } */
/*| IF expr THEN expr ELSE expr { make_cond $2 $4 $6 } */
| expr LAND expr              { make_bin_op $1 Op.bin_and $3 }
| expr LOR expr               { make_bin_op $1 Op.bin_or $3 }
| LNOT expr                   { make_un_op $1 Op.un_not $2 }
| expr EQ expr                { make_bin_op $1 Op.bin_eq $3 }
| expr NEQ expr               { make_bin_op $1 Op.bin_neq $3 }
| expr ADD expr               { make_bin_op $1 Op.bin_add $3 }
| expr SUB expr               { make_bin_op $1 Op.bin_sub $3 }
| expr MUL expr               { make_bin_op $1 Op.bin_mul $3 }
| expr DIV expr               { make_bin_op $1 Op.bin_div $3 }
| expr MOD expr               { make_bin_op $1 Op.bin_mod $3 }
| expr LTE expr               { make_bin_op $1 Op.bin_lte $3 }
| expr LT expr                { make_bin_op $1 Op.bin_lt $3 }
| expr GT expr                { make_bin_op $1 Op.bin_gt $3 }
| expr GTE expr               { make_bin_op $1 Op.bin_gte $3 }
| var                         { $1 }
;

var:
  BOOL              { make_bool $1 }
| INT               { make_int $1 }
| LIDENT            { make_var $1 }
| LPAREN abs RPAREN { $2 }
;
