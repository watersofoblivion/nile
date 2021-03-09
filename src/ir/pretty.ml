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

let pp_ground_patt fmt =
  fprintf fmt "_"

let pp_nil_patt fmt =
  fprintf fmt "nil"

let pp_unit_patt fmt =
  fprintf fmt "()"

let pp_bool_patt b fmt =
  fprintf fmt "%b" b

let pp_int_patt i fmt =
  fprintf fmt "%d" i

let pp_float_patt f fmt =
  fprintf fmt "%f" f

let pp_string_patt s fmt =
  fprintf fmt "%S" s

let pp_var_patt names sym fmt =
  let id = Sym.name_of sym names in
  fprintf fmt "%s" id

let atom_patt names patt fmt = match patt with
  | Patt.Ground -> pp_ground_patt fmt
  | Patt.Nil -> pp_nil_patt fmt
  | Patt.Unit -> pp_unit_patt fmt
  | Patt.Bool b -> pp_bool_patt b fmt
  | Patt.Int i -> pp_int_patt i fmt
  | Patt.Float f -> pp_float_patt f fmt
  | Patt.String s -> pp_string_patt s fmt
  | Patt.Var sym -> pp_var_patt names sym fmt

let pp_tuple_patt names patts fmt =

let pp_cons_patt names hd tl fmt =

let pp_constr_patt names id patt fmt =

let compound_patt names patt fmt = match patt with
  | Patt.Tuple (_, patts) -> pp_tuple_patt names patts fmt
  | Patt.Cons (hd, tl) -> pp_cons_patt names hd tl fmt
  | Patt.Constr (id, patt) -> pp_constr_patt names id patt fmt

(* Abstract syntax *)

let rec atom names a fmt = match a with
  | Anf.Unit -> pp_unit fmt
  | Anf.Bool b -> pp_bool b fmt
  | Anf.Int i -> pp_int i fmt
  | Anf.Float f -> pp_float f fmt
  | Anf.String s -> pp_string s fmt
  | Anf.Blob bs -> pp_blob bs fmt
  | Anf.Timestamp ts -> pp_timestamp ts fmt
  | Anf.Duration d -> pp_duration d fmt
  | Anf.Var sym -> pp_var names sym fmt
  | Anf.Abs (params, res, body) -> pp_abs names params res body fmt
  | Anf.Join (param, res, body) -> pp_join names params res body fmt

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

and pp_timestamp ts fmt =
  fprintf fmt "%s" ts

and pp_duration d fmt =
  fprintf fmt "%s" d

and pp_var names sym fmt =
  Sym.name_of sym names
    |> fprintf fmt "%s"

and pp_abs names params res body fmt =
  fprintf fmt "@[<hv>(%t: %t): %t =>@;<1 2>%t@]" (Patt.pp names patt) (Type.pp arg) (Type.pp res) (pp_block names body)

and pp_join names params res body fmt =
  fprintf fmt "@[<hv>(%t: %t): %t =>@;<1 2>%t@]" (Patt.pp names patt) (Type.pp arg) (Type.pp res) (pp_block names body)

and expr names e fmt = match e with
  | Anf.App (f, xs) -> pp_app names f xs fmt
  | Anf.Tail (f, x) -> pp_tail names f xs fmt
  | Anf.Jump (j, xs) -> pp_jump names j xs fmt
  | Anf.Builtin (b, xs) -> pp_builtin names b xs fmt
  | Anf.Tuple xs -> pp_tuple names xs fmt
  | Anf.Proj (tuple, field) -> pp_proj names tuple field fmt
  | Anf.Constr (id, value) -> pp_constr names id value fmt
  | Anf.Atom a -> atom names a fmt

and pp_app names f xs fmt =
  fprintf fmt "%t %t" (atom names f) (atom names x)

and pp_tail names f xs fmt =
  fprintf fmt "%t %t" (atom names f) (atom names x)

and pp_jump names j xs fmt =
  fprintf fmt "%t %t" (atom names j) (atom names x)

and pp_builtin names b xs fmt =
  fprintf fmt "%t %t" (atom names b) (atom names x)

and pp_tuple names xs fmt =
  fprintf fmt "(%t)" (atom names xs)

and pp_proj names tuple field fmt =
  fprintf fmt "%t.%d" (atom names tuple) field

and pp_constr names id value fmt =
  let id = Sym.name_of id names in
  fprintf fmt "%s" id;
  match value with
    | None -> ()
    | Some value -> fprintf fmt "%t" (atom names value)

and block names block fmt = match b with
  | Let (b, rest) -> pp_bind names b rest fmt
  | LetRec (bs, rest) -> pp_bind_rec names bs rest fmt
  | Case (scrut, clauses, _) ->  pp_case names scrut clauses fmt
  | Expr e -> expr names e fmt

and pp_bind names b rest fmt =
  fprintf fmt "@[<v>let %t in@ %t@]" (pp_binding names b) (pp_block names rest)

and pp_bind_rec names bs rest fmt =
  fprintf fmt "@[<v>let rec %t in@ %t@]" (pp_bindings names bs) (pp_block names rest)

and pp_case names scrut clauses fmt =

and pp_bindings names bs fmt =
  let pp_sep fmt _ = fprintf fmt " " in
  let pp_b b fmt = pp_binding names fmt b in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding names (sym, ty, e) fmt =
  let id = Sym.name_of sym names in
  fprintf fmt "%s: %t = %t" id (Type.pp ty) (pp_expr names e)

let top names t fmt = match t with
  | Anf.TopLet b -> fprintf fmt "@[<v>let %t@]" (pp_binding names b)
  | Anf.TopRec bs -> fprintf fmt "@[<v>let rec %t" (pp_bindings names bs)

let file names f fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top names top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt f;
  fprintf fmt "@]"
