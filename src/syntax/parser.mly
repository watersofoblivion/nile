%{
  open Format

  let make_bind kwd_loc b rest =
    let loc = Loc.span kwd_loc (Ast.loc_expr rest) in
    Ast.bind loc b rest
  let make_bind_rec kwd_loc bs rest =
    let loc = Loc.span kwd_loc (Ast.loc_expr rest) in
    Ast.bind_rec loc bs rest

  let make_param (loc, id) ty =
    Ast.param loc id ty
  let make_binding (loc, id) _ ty expr =
    (*
    let rec build_func ty args = function
      | [] -> ()
      | param :: params -> ()
    in
    *)
    let loc = Loc.span loc (Ast.loc_expr expr) in
    Ast.binding loc id ty expr

  let make_cond kwd_loc c t f =
    let loc = Loc.span kwd_loc (Ast.loc_expr f) in
    Ast.cond loc c t f

  let make_un_op op r =
    let loc = Loc.span (Op.un_loc op) (Ast.loc_expr r) in
    Ast.un_op loc op r
  let make_bin_op l op r =
    let loc = Loc.span (Ast.loc_expr l) (Ast.loc_expr r) in
    Ast.bin_op loc l op r

  let make_bool (loc, b) = Ast.bool loc b
  let make_int (loc, i) = Ast.int loc i
  let make_var (loc, id) = Ast.var loc id

  let make_prim_ty (loc, id) = match id with
    | "Int" -> Type.int loc
    | "Bool" -> Type.bool loc
    | ty -> failwith (sprintf "Unknown type %S" ty)
  let make_fun_ty a b =
    let loc = Loc.span (Type.loc a) (Type.loc b) in
    Type.func loc a b
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

%left  IN
%left  ELSE
%left  LOR
%left  LAND
%left  EQ NEQ
%left  LTE LT GT GTE
%left  ADD SUB
%left  MUL DIV MOD
%right LNOT
%right ARROW

%type <Ast.file> file
%type <Ast.expr> unit_test

%start file
%start unit_test

%%

file:
  top EOF { Ast.file $1 }
;

top:
  LET binding top      { (Ast.top_bind $1 $2) :: $3 }
| LET REC bindings top { (Ast.top_bind_rec $1 $3) :: $4 }
|                      { [] }
;

/* Exposed for unit testing only */
unit_test:
  abs EOF { $1 }
;

bindings:
  binding AND bindings { $1 :: $3 }
| binding              { [$1] }
;

binding:
  LIDENT opt_params_list COLON ty BIND abs { make_binding $1 $2 $4 $6 }
;

opt_params_list:
  LPAREN params_list RPAREN { $2 }
|                           { [] }
;

params_list:
  param COMMA params_list { $1 :: $3 }
| param                   { [$1] }
;

param:
  LIDENT COLON ty { make_param $1 $3 }
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
  LET binding IN expr         { make_bind $1 $2 $4 }
| LET REC bindings IN expr    { make_bind_rec $1 $3 $5 }
| IF expr THEN expr ELSE expr { make_cond $1 $2 $4 $6 }
| expr LOR expr               { make_bin_op $1 (Op.bin_or $2) $3 }
| expr LAND expr              { make_bin_op $1 (Op.bin_and $2) $3 }
| expr EQ expr                { make_bin_op $1 (Op.bin_eq $2) $3 }
| expr NEQ expr               { make_bin_op $1 (Op.bin_neq $2) $3 }
| expr LTE expr               { make_bin_op $1 (Op.bin_lte $2) $3 }
| expr LT expr                { make_bin_op $1 (Op.bin_lt $2) $3 }
| expr GT expr                { make_bin_op $1 (Op.bin_gt $2) $3 }
| expr GTE expr               { make_bin_op $1 (Op.bin_gte $2) $3 }
| expr ADD expr               { make_bin_op $1 (Op.bin_add $2) $3 }
| expr SUB expr               { make_bin_op $1 (Op.bin_sub $2) $3 }
| expr MUL expr               { make_bin_op $1 (Op.bin_mul $2) $3 }
| expr DIV expr               { make_bin_op $1 (Op.bin_div $2) $3 }
| expr MOD expr               { make_bin_op $1 (Op.bin_mod $2) $3 }
| LNOT expr                   { make_un_op (Op.un_not $1) $2 }
| var                         { $1 }
;

var:
  BOOL              { make_bool $1 }
| INT               { make_int $1 }
| LIDENT            { make_var $1 }
| LPAREN abs RPAREN { $2 }
;
