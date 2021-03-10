open Format
open Common

(* Types *)

let rec ty t fmt = match t with
  | Type.Unit _ -> pp_unit_ty fmt
  | Type.Bool _ -> pp_bool_ty fmt
  | Type.Int _ -> pp_int_ty fmt
  | Type.Float _ -> pp_float_ty fmt
  | Type.String _ -> pp_string_ty fmt
  | Type.Blob _ -> pp_blob_ty fmt
  | Type.Timestamp _ -> pp_timestamp_ty fmt
  | Type.Duration _ -> pp_duration_ty fmt
  | Type.Fun (_, args, ret) -> pp_fun_ty names args ret fmt
  | Type.Tuple (_, _, tys) -> pp_tuple_ty names tys fmt
  | Type.Record (_, fields) -> pp_record_ty names fields fmt
  | Type.Variant (_, constrs) -> pp_variant_ty names constrs fmt

and pp_unit_ty fmt =
  fprintf fmt "Unit"

and pp_bool_ty fmt =
  fprintf fmt "Bool"

and pp_int_ty fmt =
  fprintf fmt "Int"

and pp_float_ty fmt =
  fprintf fmt "Float"

and pp_string_ty fmt =
  fprintf fmt "String"

and pp_blob_ty fmt =
  fprintf fmt "Blob"

and pp_timestamp_ty fmt =
  fprintf fmt "Timestamp"

and pp_duration_ty fmt =
  fprintf fmt "Duration"

and pp_fun_ty names args ret fmt =
  let pp_arg fmt = function
    | Type.Fun _ as f -> fprintf fmt "(%t)" (ty f)
    | arg -> ty arg fmt
  in
  let pp_sep fmt _ = fprintf fmt " -> " in
  pp_print_list ~pp_sep pp_arg fmt args;
  fprintf fmt " -> %t" (ty ret)

and pp_tuple_ty names tys fmt =
  fprintf fmt "(@[<hv 2>";
  let pp_sep fmt _ = fprintf fmt ",@ " in
  let pp_ty fmt t = ty t fmt in
  pp_print_list ~pp_sep pp_ty fmt tys;
  fprintf fmt "@])"

and pp_record_ty names fields fmt =
  fprintf fmt "struct {";
  let pp_sep fmt _ = fprintf fmt ",@ " in
  let pp_f fmt (id, t) = fprintf fmt "%d: %t" id (ty t) in
  pp_print_list ~pp_sep pp_f fmt fields;
  fprintf fmt "}"

and pp_variant_ty names constrs fmt =
  fprintf fmt "[@<hv>";
  let pp_sep fmt _ = fprintf fmt "@ | " in
  let pp_c fmt (id, t) =
    fprintf fmt "%d" id;
    match t with
      | None -> ()
      | Some t ->
        fprintf fmt " ";
        ty t fmt
  in
  pp_print_list ~pp_sep pp_f fmt constrs;
  fprintf fmt "@]"

(* Patterns *)

let rec patt names patt fmt = match patt with
  | Patt.Ground _ -> pp_ground_patt fmt
  | Patt.Nil _ -> pp_nil_patt fmt
  | Patt.Unit _ -> pp_unit_patt fmt
  | Patt.Bool (_, b) -> pp_bool_patt b fmt
  | Patt.Int (_, i) -> pp_int_patt i fmt
  | Patt.Float (_, f) -> pp_float_patt f fmt
  | Patt.String (_, s) -> pp_string_patt s fmt
  | Patt.Var (_, sym) -> pp_var_patt names sym fmt
  | Patt.Tuple (_, _, patts) -> pp_tuple_patt names patts fmt
  | Patt.Record (_, fields, elipsis) -> pp_record_patt names fields elipsis fmt
  | Patt.Constr (_, id, value) -> pp_constr_patt names id value fmt
  | Patt.Or (_, patts) -> pp_or_patt names patts fmt

and pp_ground_patt fmt =
  fprintf fmt "_"

and pp_nil_patt fmt =
  fprintf fmt "nil"

and pp_unit_patt fmt =
  fprintf fmt "()"

and pp_bool_patt b fmt =
  fprintf fmt "%b" b

and pp_int_patt i fmt =
  fprintf fmt "%d" i

and pp_float_patt f fmt =
  fprintf fmt "%f" f

and pp_string_patt s fmt =
  fprintf fmt "%S" s

and pp_var_patt names sym fmt =
  let id = Sym.name_of sym names in
  fprintf fmt "%s" id

and pp_tuple_patt names patts fmt =

and pp_record_patt names fields elipsis fmt =

and pp_constr_patt names id value fmt =

and pp_or_patt names patts fmt =

(* Operators *)

let un op fmt = match op with
  | Op.Not _ -> fprintf fmt "!"

let bin op fmt = match op with
  | Add _ -> fprintf fmt "+"
  | Sub _ -> fprintf fmt "-"
  | Mul _ -> fprintf fmt "*"
  | Div _ -> fprintf fmt "/"
  | Mod _ -> fprintf fmt "%%"
  | And _ -> fprintf fmt "&&"
  | Or _ -> fprintf fmt "||"
  | Eq _ -> fprintf fmt "=="
  | Neq _ -> fprintf fmt "!="
  | Lte _ -> fprintf fmt "<="
  | Lt _ -> fprintf fmt "<"
  | Gt _ -> fprintf fmt ">"
  | Gte _ -> fprintf fmt ">="
  | Dot _ -> fprintf fmt "."

(* Abstract Syntax Trees *)

let rec expr names e fmt = match e with
  | Ast.Unit _ -> pp_unit fmt
  | Ast.Bool (_, b) -> pp_bool b fmt
  | Ast.Int (_, i) -> pp_int i fmt
  | Ast.Float (_, f) -> pp_float f fmt
  | Ast.String (_, _, s) -> pp_string s fmt
  | Ast.Blob (_, _, bs) -> pp_blob bs fmt
  | Ast.Timestamp (_, ts) -> pp_timestamp ts fmt
  | Ast.Duration (_, d) -> pp_duration d fmt
  | Ast.Tuple (_, exprs) -> pp_tuple names exprs fmt
  | Ast.Record (_, constr, fields) -> pp_record names constr fields fmt
  | Ast.Var (_, id) -> pp_var names id fmt
  | Ast.UnOp (_, op, r) -> pp_un_op names op r fmt
  | Ast.BinOp (_, l, op, r) -> pp_bin_op names l op r fmt
  | Ast.If (_, c, t, f) -> pp_cond names c t f fmt
  | Ast.Case (_, scrut, clauses) -> pp_case names scrut clauses fmt
  | Ast.Let (_, b, rest) -> pp_bind names b rest fmt
  | Ast.LetRec (_, bs, rest) -> pp_bind_rec names bs rest fmt
  | Ast.Abs (_, params, res, expr) -> pp_abs names params res expr fmt
  | Ast.App (_, f, xs) -> pp_app names f xs fmt

and pp_unit fmt =
  fprintf fmt "()"

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_float f fmt =
  fprintf fmt "%f" f

and pp_string s fmt =
  fprintf fmt "%S" s

and pp_blob bs fmt =
  fprintf fmt "`";
  let pp_sep fmt _ = () in
  let pp_b fmt b = fprintf fmt "\\%d" b in
  pp_print_list ~pp_sep pp_b fmt bs;
  fprintf fmt "`"

and pp_timestamp ts fmt =
  fprintf fmt "@%s" ts

and pp_duration d fmt =
  fprintf fmt "@@%s" d

and pp_tuple names exprs fmt =
  fprintf fmt "(@[<hv>";
  let pp_e fmt expr = pp_expr names expr fmt in
  let pp_sep fmt _ = fprintf fmt ",@ " in
  pp_print_list ~pp_sep pp_e fmt exprs;
  fprintf fmt "@])"

and pp_record names constr fields fmt =
  let constr = Sym.name_of constr names in
  fprintf fmt "%s{@[<hv>" constr;
  let pp_field fmt (_, name, expr) =
    let name = Sym.name_of name in
    fprintf fmt "@[<hv>%s:@ %t@]" name (pp_expr names expr)
  in
  let pp_sep fmt _ = fprintf fmt ",@ " in
  pp_print_list ~pp_sep pp_field fmt fields;
  fprintf fmt "@]}"

and pp_var names id fmt =
  Sym.name_of id names
    |> fprintf fmt "%s"

and pp_un_op names op r fmt =
  let prec = Op.un_precedence op in
  fprintf fmt "%t%t" (Op.pp_un op) (print_precedence names prec r);

and pp_bin_op names l op r fmt =
  let prec = Op.bin_precedence op in
  fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence names prec l) (Op.pp_bin op) (print_precedence names prec r)

and pp_cond names c t f fmt =
  fprintf fmt "@[<hv>@[<hv>if@;<1 2>%t@]@ @[<hv>then@;<1 2>%t@]@ @[<hv>else@;<1 2>%t@]@]" (pp_expr names c) (pp_expr names t) (pp_expr names f)

and pp_case names scrut clauses fmt =

and pp_bind names b rest fmt =
  fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding names b) (pp_expr names rest)

and pp_bind_rec names bs rest fmt =
  fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings names bs) (pp_expr names rest)

and pp_abs names patt ty res expr fmt =
  let id = Sym.name_of id names in
  fprintf fmt "(%t: %t" (Patt.pp names patt) (Type.pp ty);
  let res = pp_params names res expr fmt in
  let _ = match res with
    | Some res -> fprintf fmt "): %t" (Type.pp res)
    | None -> fprintf fmt ")"
  in
  fprintf fmt " => %t" (pp_expr names expr)

and pp_app names f x fmt =
  fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence names 0 f) (pp_expr names x)

and print_precedence names prec expr fmt =
  if prec < precedence expr
  then fprintf fmt "(%t)" (pp_expr names expr)
  else fprintf fmt "%t" (pp_expr names expr)

and pp_bindings names bs fmt =
  let pp_sep fmt _ = fprintf fmt "@ @[<hv>and " in
  let pp_b fmt b = fprintf fmt "%t@]" (pp_binding names b) in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding names (_, patt, ty, expr) fmt =
  match expr with
    | Ast.Abs (_, patt', ty, res, expr) ->
      fprintf fmt "%s(%s: %t" (Patt.pp names patt) (Patt.pp names patt') (Type.pp ty);
      let res = pp_params names res expr fmt in
      fprintf fmt ")";
      let _ = match res with
        | Some res -> fprintf fmt ": %t" (Type.pp res)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr names expr)
    | expr ->
      fprintf fmt "%t" (Patt.pp names patt);
      let _ = match ty with
        | Some ty -> fprintf fmt ": %t" (Type.pp ty)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr names expr)

and pp_params names res expr fmt = match expr with
  | Ast.Abs (_, patt, ty, res, expr) ->
    fprintf fmt ", %t: %t" (Patt.pp names patt) (Type.pp ty);
    pp_params names res expr fmt
  | _ -> res

and pp_clause names patt expr fmt =

let pp_top_val names b fmt =
  fprintf fmt "@[<hv>let %t@]" (pp_binding names b)

let pp_top_def names b fmt =
  fprintf fmt "@[<hv>let rec %t" (pp_bindings names bs)

let pp_top_ty names id ty fmt =

let top names t fmt = match t with
  | Ast.TopVal (_, b) -> pp_top_val names b fmt
  | Ast.TopDef (_, b) -> pp_top_def names b fmt
  | Ast.TopType (_, id, ty) -> pp_top_ty names id ty fmt

let pp_name names (_, id) fmt =
  fprintf fmt "%s" id

let pp_version names (_, v) fmt =
  fprintf fmt "%d" v

let pp_from names (_, src) fmt = match src with
  | Some (name, version) -> fprintf fmt "%t@v%t" (pp_name names name) (pp_version names version)
  | None -> fprintf fmt "_"

let pp_alias names (_, name, local) fmt = match local with
  | Some local -> fprintf fmt "%t -> \"%t\"" (pp_name names local) (pp_name names name)
  | None -> fprintf fmt "\"%t\"" (pp_name names name)

let pp_pkgs names (_, aliases) fmt =
  let pp_sep fmt _ = fprintf fmt "@ " in
  let pp_alias fmt alias = fprintf fmt "| %t" (pp_alias names alias) in
  fprintf fmt "@[v";
  pp_print_list ~pp_sep pp_alias fmt aliases;
  fprintf fmt "@]"

let pp_import names (_, from, pkgs) fmt =
  fprintf fmt "@[v";
  let _ =
    match from with
      | Some from ->
        pp_from names from fmt;
        fprintf fmt " "
      | None -> ()
  in
  fprintf fmt "import@ ";
  pp_pkgs names pkgs fmt;
  fprintf fmt "@]"

let pp_pkg names (_, id) fmt =
  fprintf fmt "package %t" (pp_name names id)

let file names (pkg, imports, tops) fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top names top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"
