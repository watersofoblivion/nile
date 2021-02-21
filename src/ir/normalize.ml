open Common
open Ir

(* Type Checking Environment *)

type env =
  { idx: Sym.sym;
    names: Sym.names;
    aenv: (Sym.sym * Sym.sym) list;
    tenv: Type.env }

let env tbl =
  { idx   = 0;
    names = Sym.names tbl;
    aenv  = [];
    tenv  = Type.env }

let bind id ty env = env

let (builtin_idx, builtin, builtin_aenv, builtin_tenv) =
  let fold (idx, env, aenv, tenv) (id, ty) =
    let env = Type.bind idx ty env in
    let aenv = (id, idx) :: aenv in
    let tenv = (idx, ty) :: tenv in
    (idx + 1, env, aenv, tenv)
  in
  List.fold_left fold (0, Type.env, [], []) Builtin.builtins

(* Normalization *)

let to_expr = function
  | Anf.Expr c -> c
  | _ -> failwith "Not an expression"

let to_atom = function
  | Anf.Expr (Atom a) -> a
  | _ -> failwith "Not an atom"

let rec tail_branch = function
  | Annot.If _ -> true
  | Annot.Let (_, _, rest) -> tail_branch rest
  | _ -> false

let rec of_expr idx aenv tenv join ast =
  normalize idx aenv tenv join ast (fun idx ty anf -> (idx, ty, anf))

and normalize idx aenv tenv join ast kontinue = match ast with
  | Annot.Unit -> normalize_unit idx kontinue
  | Annot.Bool (_, b) -> normalize_bool idx b kontinue
  | Annot.Int (_, i) -> normalize_int idx i kontinue
  | Annot.Var (_, id) -> normalize_var idx aenv tenv id kontinue
  | Annot.UnOp (_, op, r) -> normalize_un_op idx aenv tenv join op r kontinue
  | Annot.BinOp (_, l, op, r) -> normalize_bin_op idx aenv tenv join l op r kontinue
  | Annot.If (_, c, t, f) ->
    if tail_branch c
    then normalize_cond_with_join idx aenv tenv join c t f kontinue
    else normalize_cond idx aenv tenv join c t f kontinue
  | Annot.Let (_, (_, id, ty, expr), rest) ->
    if tail_branch expr
    then normalize_bind_with_join idx aenv tenv join id ty expr rest kontinue
    else normalize_bind idx aenv tenv join id ty expr rest kontinue
  | Annot.LetRec (_, bs, rest) -> let _ = (bs, rest) in failwith "TODO"
  | Annot.Abs (_, id, arg, res, body) -> normalize_abs idx aenv tenv join id arg res body kontinue
  | Annot.App (_, f, x) -> normalize_app idx aenv tenv join f x kontinue

and normalize_unit idx kontinue =
  Anf.unit
    |> Anf.atom
    |> Anf.expr
    |> kontinue idx Type.unit

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
