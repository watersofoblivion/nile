open Format
open Common

(* Syntax *)

type atom =
  | Bool of bool
  | Int of int
  | Var of int
  | Abs of int * Type.t * Type.t * block
and expr =
  | UnOp of Op.un * atom
  | BinOp of atom * Op.bin * atom
  | App of atom * atom
  | Atom of atom
and block =
  | Let of binding * block
  | LetRec of binding list * block
  | If of atom * block * block
  | Expr of expr
and binding = int * Type.t * expr

type top =
  | TopLet of binding
  | TopRec of binding list

type file = top list

(* Constructors *)

let bool b = Bool b
let int i = Int i
let var idx = Var idx
let abs idx arg res body = Abs (idx, arg, res, body)

let un_op op r = UnOp (op, r)
let bin_op l op r = BinOp (l, op, r)
let app f x = App (f, x)
let atom a = Atom a

let bind b rest = Let (b, rest)
let bind_rec bs rest = LetRec (bs, rest)
let cond c t f = If (c, t, f)
let expr c = Expr c

let binding idx ty expr = (idx, ty, expr)

let top_bind b = TopLet b
let top_bind_rec bs = TopRec bs

let file tops = tops

(* Type Checking Environment *)

module Checker = Check.Make (struct
  type t = int
  let compare = compare
end)

type env = Checker.env
let env = Checker.env
let (builtin_idx, builtin, builtin_aenv, builtin_tenv) =
  let fold (idx, env, aenv, tenv) (id, ty) =
    let env = Checker.bind idx ty env in
    let aenv = (id, idx) :: aenv in
    let tenv = (idx, ty) :: tenv in
    (idx + 1, env, aenv, tenv)
  in
  List.fold_left fold (0, Checker.env, [], []) Builtin.builtins

(* Normalization *)

let to_expr = function
  | Expr c -> c
  | _ -> failwith "Not an expression"

let to_atom = function
  | Expr (Atom a) -> a
  | _ -> failwith "Not an atom"

let rec tail_branch = function
  | Annot.If _ -> true
  | Annot.Let (_, _, rest) -> tail_branch rest
  | _ -> false

let rec of_expr idx aenv tenv join ast =
  normalize idx aenv tenv join ast (fun idx ty anf -> (idx, ty, anf))

and normalize idx aenv tenv join ast kontinue = match ast with
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

and normalize_bool idx b kontinue =
  bool b
    |> atom
    |> expr
    |> kontinue idx Type.bool

and normalize_int idx i kontinue =
  int i
    |> atom
    |> expr
    |> kontinue idx Type.int

and normalize_var idx aenv tenv id kontinue =
  let alpha = List.assoc id aenv in
  let ty = List.assoc alpha tenv in
  var alpha
    |> atom
    |> expr
    |> kontinue idx ty

and normalize_un_op idx aenv tenv join op r kontinue =
  normalize_and_bind idx aenv tenv join r (fun idx _ r ->
    let ty = match op with
      | Op.Not -> Type.bool
    in
    un_op op (to_atom r)
      |> expr
      |> kontinue idx ty
  )

and normalize_bin_op idx aenv tenv join l op r kontinue =
  normalize_and_bind idx aenv tenv join l (fun idx _ l ->
    normalize_and_bind idx aenv tenv join r (fun idx _ r ->
      let ty = match op with
        | Op.Add | Op.Sub | Op.Mul | Op.Div | Op.Mod -> Type.int
        | _ -> Type.bool
      in
      bin_op (to_atom l) op (to_atom r)
        |> expr
        |> kontinue idx ty
    )
  )

and normalize_cond_with_join idx aenv tenv join c t f kontinue =
  let (idx, ty, t, f) = normalize_tf idx aenv tenv join t f in
  let join =
    cond (var idx) t f
      |> abs idx Type.bool ty
      |> atom
      |> binding (idx + 1) (Type.func Type.bool ty)
  in
  let (idx, _, c) = normalize (idx + 2) aenv tenv (Some ((idx + 1), ty)) c kontinue in
  (idx, ty, bind join c)

and normalize_cond idx aenv tenv join c t f kontinue =
  normalize_and_bind idx aenv tenv join c (fun idx _ c ->
    let (idx, ty, t, f) = normalize_tf idx aenv tenv join t f in
    cond (to_atom c) t f
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
        |> binding idx ty
    in
    let aenv = (id, idx) :: aenv in
    let tenv = (idx, ty) :: tenv in
    let idx = idx + 1 in
    let (idx, ty, rest) = normalize idx aenv tenv join rest kontinue in
    (idx, ty, bind b rest)
  )

and normalize_abs idx aenv tenv join id arg res body kontinue =
  let aenv = (id, idx) :: aenv in
  let tenv = (idx, arg) :: tenv in
  let (idx', _, body) = of_expr (idx + 1) aenv tenv join body in
  let ty = Type.func arg res in
  abs idx arg res body
    |> atom
    |> expr
    |> kontinue idx' ty

and normalize_app idx aenv tenv join f x kontinue =
  normalize_and_bind idx aenv tenv join x (fun idx _ x ->
    normalize_and_bind idx aenv tenv join f (fun idx ty f ->
      let ty = match ty with
        | Type.Fun (_, ty) -> ty
        | _ -> failwith "Not a function, cannot apply"
      in
      app (to_atom f) (to_atom x)
        |> expr
        |> kontinue idx ty
    )
  )

and normalize_and_join idx aenv tenv join idx_j ty_j ast =
  normalize_and_bind idx aenv tenv join ast (fun idx _ x ->
    let j = var idx_j in
    let x =
      to_atom x
        |> app j
        |> expr
    in
    (idx, ty_j, x)
  )

and normalize_and_bind idx aenv tenv join ast kontinue =
  normalize idx aenv tenv join ast (fun idx ty anf ->
    match anf with
      | Expr (Atom _) -> kontinue idx ty anf
      | Expr e ->
        let b = binding idx ty e in
        let (idx, ty, rest) =
          var idx
            |> atom
            |> expr
            |> kontinue (idx + 1) ty
        in
        (idx, ty, bind b rest)
      | _ -> failwith "Not an atom or expression"
    )

let rec of_top idx aenv tenv join ast =
  normalize_top idx aenv tenv join ast (fun idx ty anf -> (idx, ty, anf))

and normalize_top idx aenv tenv join ast kontinue = match ast with
  | Unannot.Bool (_, b) -> normalize_top_bool idx b kontinue
  | Unannot.Int (_, i) -> normalize_top_int idx i kontinue
  | Unannot.Var (_, id) -> normalize_top_var idx aenv tenv id kontinue
  | Unannot.UnOp (_, op, r) -> normalize_top_un_op idx aenv tenv join op r kontinue
  | Unannot.BinOp (_, l, op, r) -> normalize_top_bin_op idx aenv tenv join l op r kontinue
  | Unannot.If (_, c, t, f) ->
    if tail_branch c
    then normalize_top_cond_with_join idx aenv tenv join c t f kontinue
    else normalize_top_cond idx aenv tenv join c t f kontinue
  | Unannot.Let (_, (_, id, ty, expr), rest) ->
    if tail_branch expr
    then normalize_top_bind_with_join idx aenv tenv join id ty expr rest kontinue
    else normalize_top_bind idx aenv tenv join id ty expr rest kontinue
  | Unannot.LetRec (_, bs, rest) -> let _ = (bs, rest) in failwith "TODO"
  | Unannot.Abs (_, id, ty, res, body) -> normalize_top_abs idx aenv tenv join id ty res body kontinue
  | Unannot.App (_, f, x) -> normalize_top_app idx aenv tenv join f x body kontinue

and normalize_top_bool idx b kontinue =
  bool b
    |> atom
    |> expr
    |> kontinue idx Type.bool

and normalize_top_int idx i kontinue =
  int i
    |> atom
    |> expr
    |> kontinue idx Type.int

and normalize_top_var idx aenv tenv id kontinue =
  let alpha = List.assoc id aenv in
  let ty = List.assoc alpha tenv in
  var alpha
    |> atom
    |> expr
    |> kontinue idx ty

and normalize_top_un_op idx aenv tenv join op r kontinue =
  normalize_top_and_bind idx aenv tenv join r (fun idx _ r ->
    let ty = match op with
      | Op.Not -> Type.bool
    in
    un_op op (to_atom r)
      |> expr
      |> kontinue idx ty
  )

and normalize_top_bin_op idx aenv tenv join l op r kontinue =
and normalize_top_cond_with_join idx aenv tenv join c t f kontinue =
and normalize_top_cond idx aenv tenv join c t f kontinue =
and normalize_top_bind_with_join idx aenv tenv join id ty expr rest kontinue =
and normalize_top_bind idx aenv tenv join id ty expr rest kontinue =
and normalize_top_abs idx aenv tenv join id ty res body kontinue =
and normalize_top_app idx aenv tenv join f x kontinue =
and normalize_top_and_join idx aenv tenv join idx_j ty_j ast =

and normalize_top_and_bind idx aenv tenv join ast kontinue =
  normalize_top idx aenv tenv join ast (fun idx ty anf ->
    match anf with
      | Expr (Atom _) -> kontinue idx ty anf
      | Expr e ->
        let b = binding idx ty e in
        let (idx, ty, rest) =
          var idx
            |> atom
            |> expr
            |> kontinue (idx + 1) ty
        in
        (idx, ty, bind b rest)
      | _ -> failwith "Not an atom or expression"
    )

let rec of_file idx aenv tenv = function
  | [] -> (idx, [])
  | top :: file ->
    let (idx, aenv, tenv, tops) = of_top idx aenv tenv top in
    let (idx, file) = of_file idx aenv tenv file in
    (idx, tops @ file)

(* Pretty-Printing *)

let rec pp_atom atom fmt = match atom with
  | Bool b -> fprintf fmt "%b" b
  | Int i -> fprintf fmt "%d" i
  | Var idx -> fprintf fmt "$%d" idx
  | Abs (id, arg, res, body) -> pp_abs id arg res body fmt

and pp_abs id arg res body fmt =
  fprintf fmt "@[<hv>($%d: %t): %t =>@;<1 2>%t@]" id (Type.pp arg) (Type.pp res) (pp_block body)

and pp_expr expr fmt = match expr with
  | UnOp (op, r) -> pp_un_op op r fmt
  | BinOp (l, op, r) -> pp_bin_op l op r fmt
  | App (f, x) -> pp_app f x fmt
  | Atom atom -> pp_atom atom fmt

and pp_un_op op r fmt =
  fprintf fmt "%t%t" (Op.pp_un op) (pp_atom r)

and pp_bin_op l op r fmt =
  fprintf fmt "%t %t %t" (pp_atom l) (Op.pp_bin op) (pp_atom r)

and pp_app f x fmt =
  fprintf fmt "%t %t" (pp_atom f) (pp_atom x)

and pp_block block fmt = match block with
  | Let (b, rest) -> pp_bind b rest fmt
  | LetRec (bs, rest) -> pp_bind_rec bs rest fmt
  | If (c, t, f) ->  pp_cond c t f fmt
  | Expr expr -> pp_expr expr fmt

and pp_bind b rest fmt =
  fprintf fmt "@[<v>let %t in@ %t@]" (pp_binding b) (pp_block rest)

and pp_bind_rec bs rest fmt =
  fprintf fmt "@[<v>let rec %t in@ %t@]" (pp_bindings bs) (pp_block rest)

and pp_cond c t f fmt =
  fprintf fmt "@[<v>if %t@ then %t@ else %t@]" (pp_atom c) (pp_block t) (pp_block f)

and pp_bindings bs fmt =
  let pp_sep fmt _ = fprintf fmt " " in
  let pp_b b fmt = pp_binding fmt b in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding (id, ty, e) fmt =
  fprintf fmt "$%d: %t = %t" id (Type.pp ty) (pp_expr e)

let pp_top top fmt = match top with
  | TopLet b -> fprintf fmt "@[<v>let %t@]" (pp_binding b)
  | TopRec bs -> fprintf fmt "@[<v>let rec %t" (pp_bindings bs)

let pp_file file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"

(* Type Checking *)

let rec type_of_atom env = function
  | Bool _ -> Type.bool
  | Int _ -> Type.int
  | Var idx -> type_of_var env idx
  | Abs (idx, ty, res, body) -> type_of_abs env idx ty res body

and type_of_var env idx =
  try Checker.lookup idx env
  with Not_found -> Checker.unbound_identifier idx

and type_of_abs env idx ty res body =
  let env = Checker.bind idx ty env in
  let res' = type_of_block env body in
  if Type.equal res res'
  then res
  else Checker.declaration_mismatch idx res res'

and type_of_expr env = function
  | UnOp (op, r) -> type_of_un_op env op r
  | BinOp (l, op, r) -> type_of_bin_op env l op r
  | App (f, x) -> type_of_app env f x
  | Atom atom -> type_of_atom env atom

and type_of_un_op env op r =
  let r = type_of_atom env r in
  Op.type_of_un op r

and type_of_bin_op env l op r =
  let l = type_of_atom env l in
  let r = type_of_atom env r in
  Op.type_of_bin l op r

and type_of_app env f x =
  let x = type_of_atom env x in
  match type_of_atom env f with
    | Type.Fun (arg, res) ->
      if Type.equal arg x
      then res
      else Checker.invalid_args arg x
    | ty -> Checker.cannot_apply ty

and type_of_block env = function
  | Let ((idx, ty, expr), rest) -> type_of_bind env idx ty expr rest
  | LetRec (bs, rest) -> type_of_bind_rec env bs rest
  | If (c, t, f) -> type_of_cond env c t f
  | Expr expr -> type_of_expr env expr

and type_of_bind env idx ty expr rest =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then
    let env = Checker.bind idx ty env in
    type_of_block env rest
  else Checker.declaration_mismatch idx ty expr

and type_of_bind_rec env bs rest =
  let fold env (idx, ty, _) = Checker.bind idx ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (idx, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Checker.declaration_mismatch idx ty expr
    in
    List.iter iter bs
  in
  type_of_block env rest

and type_of_cond env c t f = match type_of_atom env c with
  | Type.Bool ->
    let t = type_of_block env t in
    let f = type_of_block env f in
    if Type.equal t f
    then t
    else Checker.conditional_branch_mismatch t f
  | ty -> Checker.invalid_condition ty

let type_of_top_bind env idx ty expr =
  let expr = type_of_expr env expr in
  if Type.equal ty expr
  then Checker.bind idx ty env
  else Checker.declaration_mismatch idx ty expr

let type_of_top_bind_rec env bs =
  let fold env (idx, ty, _) = Checker.bind idx ty env in
  let env = List.fold_left fold env bs in
  let _ =
    let iter (idx, ty, expr) =
      let expr = type_of_expr env expr in
      if Type.equal ty expr
      then ()
      else Checker.declaration_mismatch idx ty expr
    in
    List.iter iter bs
  in
  env

let type_of_top env = function
  | TopLet (idx, ty, expr) -> type_of_top_bind env idx ty expr
  | TopRec bs -> type_of_top_bind_rec env bs

let type_of_file = List.fold_left type_of_top
