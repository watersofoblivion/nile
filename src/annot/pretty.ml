open Format

(* Types *)

let rec ty t fmt = match t with
  | Type.Unit -> pp_unit_ty fmt
  | Type.Bool -> pp_bool_ty fmt
  | Type.Int -> pp_int_ty fmt
  | Type.Float -> pp_float_ty fm
  | Type.String -> pp_string_ty fmt
  | Type.Blob -> pp_blob_ty fmt
  | Type.Timestamp -> pp_timestamp_ty fmt
  | Type.Duration -> pp_duration_ty fmt
  | Type.Fun (args, ret) -> pp_fun_ty args ret fmt
  | Type.Tuple (_, tys) -> pp_tuple_ty tys fmt
  | Type.Record fields -> pp_record_ty fields fmt
  | Type.Variant constrs -> pp_variant_ty constrs fmt

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

and pp_fun_ty args ret fmt =
  let pp_arg fmt = function
    | Type.Fun _ as f -> fprintf fmt "(%t)" (ty f)
    | arg -> ty arg fmt
  in
  let pp_sep fmt _ = fprintf fmt " -> " in
  pp_print_list ~pp_sep pp_arg fmt args;
  fprintf fmt " -> %t" (ty ret)

and pp_tuple_ty tys fmt =
  fprintf fmt "(@[<hv 2>";
  let pp_sep fmt _ = fprintf fmt ",@ " in
  let pp_ty fmt t = ty t fmt in
  pp_print_list ~pp_sep pp_ty fmt tys;
  fprintf fmt "@])"

and pp_record_ty fields fmt =
  fprintf fmt "struct {";
  let pp_sep fmt _ = fprintf fmt ",@ " in
  let pp_f fmt (id, t) = fprintf fmt "%d: %t" id (ty t) in
  pp_print_list ~pp_sep pp_f fmt fields;
  fprintf fmt "}"

and pp_variant_ty constrs fmt =
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

let rec pp names patt fmt = match patt with
  | Patt.Ground -> pp_ground_patt fmt
  | Patt.Nil -> pp_nil_patt fmt
  | Patt.Unit -> pp_unit_patt fmt
  | Patt.Bool b -> pp_bool_patt b fmt
  | Patt.Int i -> pp_int_patt i fmt
  | Patt.Float f -> pp_float_patt f fmt
  | Patt.String s -> pp_string_patt s fmt
  | Patt.Var sym -> pp_var_patt names sym fmt
  | Patt.Tuple (_, patts) -> pp_tuple_patt names patts fmt
  | Patt.Record fields -> pp_record_patt names fields fmt
  | Patt.Cons (hd, tl) -> pp_cons_patt names hd tl fmt
  | Patt.Or patts -> pp_or_patt names patts fmt

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

and pp_record_patt names fields fmt =

and pp_cons_patt names hd tl fmt =

and pp_or_patt names patts fmt =

(* Operators *)

let un op fmt = match op with
  | Op.Not -> fprintf fmt "!"

let bin op fmt = match op with
  | Op.Add -> fprintf fmt "+"
  | Op.Sub -> fprintf fmt "-"
  | Op.Mul -> fprintf fmt "*"
  | Op.Div -> fprintf fmt "/"
  | Op.Mod -> fprintf fmt "%%"
  | Op.And -> fprintf fmt "&&"
  | Op.Or -> fprintf fmt "||"
  | Op.Eq -> fprintf fmt "=="
  | Op.Neq -> fprintf fmt "!="
  | Op.Lte -> fprintf fmt "<="
  | Op.Lt -> fprintf fmt "<"
  | Op.Gt -> fprintf fmt ">"
  | Op.Gte -> fprintf fmt ">="
  | Op.Dot -> fprintf fmt "."
  | Op.Cons -> fprintf fmt "::"

(* Abstract Syntax Trees *)

let rec expr names e fmt = match e with
  | Ast.Unit -> pp_unit fmt
  | Ast.Bool b -> pp_bool b fmt
  | Ast.Int i -> pp_int i fmt
  | Ast.Float f -> pp_float f fmt
  | Ast.String (_, s) -> pp_string s fmt
  | Ast.Blob (_, bs) -> pp_blob bs fmt
  | Ast.Timestamp ts -> pp_timestamp ts fmt
  | Ast.Duration d -> pp_duration fmt
  | Ast.Tuple exprs -> pp_tuple names exprs fmt
  | Ast.Constr (id, v) -> pp_constr names id v fmt
  | Ast.Var sym -> pp_var names sym fmt
  | Ast.Case (scrut, clauses, _) -> pp_case names scrut clauses fmt
  | Ast.Let (b, rest) -> pp_bind names b rest fmt
  | Ast.LetRec (bs, rest) -> pp_bind_rec names bs rest fmt
  | Ast.Proj (expr, n) -> pp_proj names expr n fmt
  | Ast.Abs (_, params, res, expr) -> pp_abs names params res expr fmt
  | Ast.App (_, f, xs) -> pp_app names f xs fmt
  | Ast.Builtin (f, xs) -> pp_builtin f xs fmt

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
  let pp_b fmt b = fprintf fmt "\\%d" b in
  let pp_sep fmt _ = () in
  pp_print_list ~pp_sep pp_b fmt bs;
  fprintf fmt "`"

and pp_timestamp ts fmt =
  fprintf fmt "@%s" ts

and pp_duration d fmt =
  fprintf fmt "@@%s" d

and pp_tuple names exprs fmt =

and pp_constr names id v fmt =

and pp_var names sym fmt =
  Sym.name_of sym names
    |> fprintf fmt "%s"

(* and pp_case names scrut clauses fmt = *)
and pp_case _ _ _ _ = failwith "TODO"

and pp_bind names b rest fmt =
  fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding names b) (pp_expr names rest)

and pp_bind_rec names bs rest fmt =
  fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings names bs) (pp_expr names rest)

and pp_proj names expr n fmt =

and pp_abs names params res expr fmt =
  let id = Sym.name_of id names in
  fprintf fmt "(%t: %t" (Patt.pp names patt) (Type.pp ty);
  let res = pp_params names res expr fmt in
  fprintf fmt "): %t => %t" (Type.pp res) (pp_expr names expr)

and pp_app names f xs fmt =
  fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence names 0 f) (pp_expr names x)

and pp_builtin names f xs fmt =
  fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence names 0 f) (pp_expr names x)

and pp_builtin names f x fmt =

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
    | Abs (_, patt', ty, res, expr) ->
      fprintf fmt "%s(%s: %t" (Patt.pp names patt) (Patt.pp names patt') (Type.pp ty);
      let res = pp_params names res expr fmt in
      fprintf fmt "): %t =@;<1 2>%t" (Type.pp res) (pp_expr names expr)
    | expr ->
      fprintf fmt "%t: %t =@;<1 2>%t" (Patt.pp names patt) (Type.pp ty) (pp_expr names expr)

and pp_params names res expr fmt = match expr with
  | Abs (_, patt, ty, res, expr) ->
    fprintf fmt ", %t: %t" (Patt.pp names patt) (Type.pp ty);
    pp_params names res expr fmt
  | _ -> res

(* and pp_clause names patt expr fmt = *)
and pp_clause _ _ _ _ = failwith "TODO"

let pp_top_bind names b fmt =
  fprintf fmt "@[<hv>let %t@]" (pp_binding names b)

let pp_top_bind_rec names bs fmt =
  fprintf fmt "@[<hv>let rec %t" (pp_bindings names bs)

let top names t fmt = match t with
  | Ast.TopLet (_, b) -> pp_top_bind names b fmt
  | Ast.TopRec (_, bs) -> pp_top_bind_rec names bs fmt

let pkg names p fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt t = top names t fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt p;
  fprintf fmt "@]"
