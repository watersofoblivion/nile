open Common
open Ir

(* Type Checking Environment *)

type env =
  { idx: Sym.s;
    syms: Sym.t;
    aenv: (Sym.s * Sym.s) list;
    tenv: Check.env }

let env tbl =
  { idx  = 0;
    syms = tbl;
    aenv = [];
    tenv = Check.env }

let bind id ty env = env

let (builtin_idx, builtin, builtin_aenv, builtin_tenv) =
  let fold (idx, env, aenv, tenv) (id, ty) =
    let env = Check.bind idx ty env in
    let aenv = (id, idx) :: aenv in
    let tenv = (idx, ty) :: tenv in
    (idx + 1, env, aenv, tenv)
  in
  List.fold_left fold (0, Check.env, [], []) Builtin.builtins

(* Normalization *)

let to_expr = function
  | Anf.Expr c -> c
  | _ -> failwith "Not an expression"

let to_atom = function
  | Anf.Expr (Atom a) -> a
  | _ -> failwith "Not an atom"

let rec tail_branch = function
  | Ast.If _ -> true
  | Ast.Let (_, _, rest) -> tail_branch rest
  | _ -> false

let rec of_expr idx aenv tenv join ast =
  normalize idx aenv tenv join ast (fun idx ty anf -> (idx, ty, anf))

and normalize idx aenv tenv join ast kontinue = match ast with
  | Ast.Bool (_, b) -> normalize_bool idx b kontinue
  | Ast.Int (_, i) -> normalize_int idx i kontinue
  | Ast.Var (_, id) -> normalize_var idx aenv tenv id kontinue
  | Ast.UnOp (_, op, r) -> normalize_un_op idx aenv tenv join op r kontinue
  | Ast.BinOp (_, l, op, r) -> normalize_bin_op idx aenv tenv join l op r kontinue
  | Ast.If (_, c, t, f) ->
    if tail_branch c
    then normalize_cond_with_join idx aenv tenv join c t f kontinue
    else normalize_cond idx aenv tenv join c t f kontinue
  | Ast.Let (_, (_, id, ty, expr), rest) ->
    if tail_branch expr
    then normalize_bind_with_join idx aenv tenv join id ty expr rest kontinue
    else normalize_bind idx aenv tenv join id ty expr rest kontinue
  | Ast.LetRec (_, bs, rest) -> let _ = (bs, rest) in failwith "TODO"
  | Ast.Abs (_, id, arg, res, body) -> normalize_abs idx aenv tenv join id arg res body kontinue
  | Ast.App (_, f, x) -> normalize_app idx aenv tenv join f x kontinue

and normalize_bool idx b kontinue =
  Anf.bool b
    |> Anf.atom
    |> Anf.expr
    |> kontinue idx Type.bool

and normalize_int idx i kontinue =
  Anf.int i
    |> Anf.atom
    |> Anf.expr
    |> kontinue idx Type.int

and normalize_var idx aenv tenv id kontinue =
  let alpha = List.assoc id aenv in
  let ty = List.assoc alpha tenv in
  Anf.var alpha
    |> Anf.atom
    |> Anf.expr
    |> kontinue idx ty

and normalize_un_op idx aenv tenv join op r kontinue =
  normalize_and_bind idx aenv tenv join r (fun idx _ r ->
    let ty = match op with
      | Op.Not -> Type.bool
    in
    Anf.un_op op (to_atom r)
      |> Anf.expr
      |> kontinue idx ty
  )

and normalize_bin_op idx aenv tenv join l op r kontinue =
  normalize_and_bind idx aenv tenv join l (fun idx _ l ->
    normalize_and_bind idx aenv tenv join r (fun idx _ r ->
      let ty = match op with
        | Op.Add | Op.Sub | Op.Mul | Op.Div | Op.Mod -> Type.int
        | _ -> Type.bool
      in
      Anf.bin_op (to_atom l) op (to_atom r)
        |> Anf.expr
        |> kontinue idx ty
    )
  )

and normalize_cond_with_join idx aenv tenv join c t f kontinue =
  let (idx, ty, t, f) = normalize_tf idx aenv tenv join t f in
  let join =
    Anf.cond (Anf.var idx) t f
      |> Anf.abs idx Type.bool ty
      |> Anf.atom
      |> Anf.binding (idx + 1) (Type.func Type.bool ty)
  in
  let (idx, _, c) = normalize (idx + 2) aenv tenv (Some ((idx + 1), ty)) c kontinue in
  (idx, ty, Anf.bind join c)

and normalize_cond idx aenv tenv join c t f kontinue =
  normalize_and_bind idx aenv tenv join c (fun idx _ c ->
    let (idx, ty, t, f) = normalize_tf idx aenv tenv join t f in
    Anf.cond (to_atom c) t f
      |> kontinue idx ty
  )

and normalize_tf idx aenv tenv join t f = match join with
  | Some (idx_j, ty_j) ->
    let (idx, _, t) = normalize_and_join idx aenv tenv join idx_j ty_j t in
    let (idx, _, f) = normalize_and_join idx aenv tenv join idx_j ty_j f in
    (idx, ty_j, t, f)
  | None ->
    let (idx, ty, t) = of_expr idx aenv tenv None t in
    let (idx, _, f) = of_expr idx aenv tenv None f in
    (idx, ty, t, f)

(* and normalize_bind_with_join idx aenv tenv join id ty expr rest kontinue = *)
and normalize_bind_with_join _ _ _ _ _ _ _ _ _ =
  failwith "TODO"

and normalize_bind idx aenv tenv join id ty expr rest kontinue =
  normalize idx aenv tenv join expr (fun idx _ expr ->
    let b =
      expr
        |> to_expr
        |> Anf.binding idx ty
    in
    let aenv = (id, idx) :: aenv in
    let tenv = (idx, ty) :: tenv in
    let idx = idx + 1 in
    let (idx, ty, rest) = normalize idx aenv tenv join rest kontinue in
    (idx, ty, Anf.bind b rest)
  )

and normalize_abs idx aenv tenv join id arg res body kontinue =
  let aenv = (id, idx) :: aenv in
  let tenv = (idx, arg) :: tenv in
  let (idx', _, body) = of_expr (idx + 1) aenv tenv join body in
  let ty = Type.func arg res in
  Anf.abs idx arg res body
    |> Anf.atom
    |> Anf.expr
    |> kontinue idx' ty

and normalize_app idx aenv tenv join f x kontinue =
  normalize_and_bind idx aenv tenv join x (fun idx _ x ->
    normalize_and_bind idx aenv tenv join f (fun idx ty f ->
      let ty = match ty with
        | Type.Fun (_, ty) -> ty
        | _ -> failwith "Not a function, cannot apply"
      in
      Anf.app (to_atom f) (to_atom x)
        |> Anf.expr
        |> kontinue idx ty
    )
  )

and normalize_and_join idx aenv tenv join idx_j ty_j ast =
  normalize_and_bind idx aenv tenv join ast (fun idx _ x ->
    let j = var idx_j in
    let x =
      to_atom x
        |> Anf.app j
        |> Anf.expr
    in
    (idx, ty_j, x)
  )

and normalize_and_bind idx aenv tenv join ast kontinue =
  normalize idx aenv tenv join ast (fun idx ty anf ->
    match anf with
      | Anf.Expr (Anf.Atom _) -> kontinue idx ty anf
      | Anf.Expr e ->
        let b = binding idx ty e in
        let (idx, ty, rest) =
          Anf.var idx
            |> Anf.atom
            |> Anf.expr
            |> kontinue (idx + 1) ty
        in
        (idx, ty, Anf.bind b rest)
      | _ -> failwith "Not an atom or expression"
    )

let of_top _ _ _ _ = failwith "TODO"

let rec of_file idx aenv tenv = function
  | [] -> (idx, [])
  | top :: file ->
    let (idx, aenv, tenv, tops) = of_top idx aenv tenv top in
    let (idx, file) = of_file idx aenv tenv file in
    (idx, List.rev_append tops file)

(* Annotation *)

let builtin =
  let fold env (id, ty) = Check.bind id ty env in
  List.fold_left fold Check.env Builtin.builtins

and annotate_var env id =
  try (var id, Check.lookup id env)
  with Not_found -> Check.unbound_identifier id

and annotate_un_op env op r =
  let (r, r_ty) = annotate_expr env r in
  (un_op op r, Op.type_of_un op r_ty)

and annotate_bin_op env l op r =
  let (l, l_ty) = annotate_expr env l in
  let (r, r_ty) = annotate_expr env r in
  (bin_op l op r, Op.type_of_bin l_ty op r_ty)

and annotate_cond env c t f =
  let (c, ty) = annotate_expr env c in
  match ty with
    | Type.Bool ->
      let (t, t_ty) = annotate_expr env t in
      let (f, f_ty) = annotate_expr env f in
      if Type.equal t_ty f_ty
      then (cond c t f, t_ty)
      else Check.conditional_branch_mismatch t_ty f_ty
    | ty -> Check.invalid_condition ty

and annotate_bind env id ty expr rest =
  let (expr, expr_ty) = annotate_expr env expr in
  let env = match ty with
    | Some ty ->
      if Type.equal expr_ty ty
      then Check.bind id expr_ty env
      else Check.declaration_mismatch id ty expr_ty
    | None -> Check.bind id expr_ty env
  in
  let (rest, rest_ty) = annotate_expr env rest in
  let b = binding id expr_ty expr in
  (bind b rest, rest_ty)

and annotate_bind_rec env bs rest =
  let fold env (_, id, ty, _) = match ty with
    | Some ty -> Check.bind id ty env
    | None -> Check.annotation_required id
  in
  let env = List.fold_left fold env bs in
  let bs =
    let map (loc, id, ty, expr) =
      let (expr, expr_ty) = annotate_expr env expr in
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then binding id expr_ty expr
          else Check.declaration_mismatch id ty expr_ty
        | None -> Check.annotation_required id
    in
    List.map map bs
  in
  let (rest, rest_ty) = annotate_expr env rest in
  (bind_rec bs rest, rest_ty)

and annotate_abs env id ty res expr =
  let (expr, expr_ty) =
    let env = Check.bind id ty env in
    annotate_expr env expr
  in
  let res = match res with
    | Some res ->
      if Type.equal res expr_ty
      then expr_ty
      else Check.result_mismatch res expr_ty
    | None -> expr_ty
  in
  (abs id ty res expr, Type.func ty res)

and annotate_app env f x =
  let (f, f_ty) = annotate_expr env f in
  let (x, res) = match f_ty with
    | Type.Fun (arg, res) ->
        let (x, x_ty) = annotate_expr env x in
        if Type.equal arg x_ty
        then (x, res)
        else Check.invalid_args arg x_ty
    | _ -> Check.cannot_apply f_ty
  in
  (app f x, res)

let annotate_top_bind env id ty expr =
  let (expr, expr_ty) = annotate_expr env expr in
  let env = match ty with
    | Some ty ->
      if Type.equal expr_ty ty
      then Check.bind id expr_ty env
      else Check.declaration_mismatch id ty expr_ty
    | None -> Check.bind id expr_ty env
  in
  let b = binding id expr_ty expr in
  (top_bind b, env)

let annotate_top_bind_rec env bs =
  let fold env (_, id, ty, _) = match ty with
    | Some ty -> Check.bind id ty env
    | None -> Check.annotation_required id
  in
  let env = List.fold_left fold env bs in
  let bs =
    let map (loc, id, ty, expr) =
      let (expr, expr_ty) = annotate_expr env expr in
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then binding id expr_ty expr
          else Check.declaration_mismatch id ty expr_ty
        | None -> Check.annotation_required id
    in
    List.map map bs
  in
  (top_bind_rec bs, env)

let annotate_top env = function
  | Ast.TopLet (_, (_, id, ty, expr)) -> annotate_top_bind env id ty expr
  | Ast.TopRec (_, bs) -> annotate_top_bind_rec env bs

let rec annotate_file env = function
  | [] -> []
  | top :: file ->
    let (top, env) = annotate_top env top in
    let file = annotate_file env file in
    top :: file
