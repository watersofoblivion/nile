open Format
open Common
open Ir

(* Syntax *)

type expr =
  | Unit
  | Bool of { b: bool }
  | Int of { i: int }
  | Float of { f: float }
  | Rune of { r: bytes }
  | String of { length: int; s: string }
  | Byte of { b: bytes }
  | Blob of { length: int; bs: bytes }
  | Timestamp of { ts: string }
  | Duration of { d: string }
  | Tuple of { exprs: expr list; ty: Type.t }
  | Record of { constr: Sym.sym option; fields: field list; ty: Type.t }
  | Constr of { name: Sym.sym; args: expr list; ty: Type.t }
  | Var of { id: Sym.sym }
  | UnOp of { op: Op.un; lhs: expr }
  | BinOp of { op: Op.bin; lhs: expr; rhs: expr }
  | Slice of { expr: expr; start: expr option; stop: expr option }
  | Index of { expr: expr; idx: expr }
  | Case of { scrut: expr; clauses: clause list; res: Type.t }
  | Let of { binding: binding; scope: expr }
  | LetRec of { bindings: binding list; scope: expr }
  | TupleProj of { tuple: expr; index: int }
  | RecordProj of { record: expr; field: Sym.sym }
  | Abs of { arity: int; params: param list; res: Type.t; body: expr }
  | App of { arity: int; fn: expr; args: expr list }
and param = Param of { name: Patt.t; ty: Type.t }
and binding = Binding of { name: Patt.t; ty: Type.t; body: expr }
and clause = Clause of { patt: Patt.t; body: expr }

type top =
  | TopLet of { binding: binding }
  | TopRec of { bindings: binding list }

type pkg = Package of { name: Sym.sym; tops: top list }

(* Constructors *)

let unit = Unit
let bool b = Bool { b }
let int i = Int { i }
let float f = Float { f }
let rune r = Rune { r }
let string s = String { length = String.length s; s }
let byte b = Byte { b }
let blob bs = Blob { length = Bytes.length bs; bs }
let timestmap ts = Timestamp { ts }
let duration d = Duration { d }
let tuple exprs ty = Tuple { exprs; ty }
let record constr fields ty = Record { constr; fields; ty }
let constr name args ty = Constr { name; args; ty }
let var sym = Var { sym }
let un_op op prec lhs = UnOp { op; prec; lhs }
let bin_op op prec lhs rhs = BinOp { op; prec; lhs; rhs }
let slice expr start stop = Slice { expr; start; stop }
let index expr idx = Index { expr; idx }
let case scrut clauses res = Case { scrut; clauses; res }
let bind binding scope = Let { binding; scope }
let bind_rec bindings scope = LetRec { bindings; scope }
let tuple_proj tuple index = TupleProj { tuple; index }
let record_proj record field = RecordProj { record; field }
let abs params res body = Abs { arity = List.length params; params; res; body }
let app fn args = App { arity = List.length args; fn; args }

let param name ty = Param { name; ty }
let binding name ty body = Binding { name; ty; body }
let clause patt body = Clause of { patt; body }

let top_bind binding = TopLet { binding }
let top_bind_rec bindings = TopRec { bindings }

let pkg name tops = Package { name; tops }

(* Annotation *)

(* let builtin =
  let fold env (id, ty) = Type.bind id ty env in
  List.fold_left fold Type.env Builtin.builtins *)

let bindings_env =
  let fold env (_, patt, ty, _) = match ty with
    | Some ty -> Type.bind patt ty env
    | None -> Type.annotation_required patt
  in
  List.fold_left fold

let rec annotate_expr env expr kontinue = match expr with
  | Ast.Unit _ -> annotate_unit kontinue
  | Ast.Bool (_, b) -> annotate_bool b kontinue
  | Ast.Int (_, i) -> annotate_int i kontinue
  | Ast.Float (_, f) -> annotate_float f kontinue
  | Ast.String (_, len, s) -> annotate_string len s kontinue
  | Ast.Blob (_, len, bs) -> annotate_blob len bs kontinue
  | Ast.Timestamp (_, ts) -> annotate_timestamp ts kontinue
  | Ast.Duration (_, d) -> annotate_duration d kontinue
  | Ast.Tuple (_, exprs) -> annotate_tuple env exprs kontinue
  | Ast.Record (_, constr, fields) -> annotate_record env constr fields kontinue
  | Ast.Var (_, sym) -> annotate_var env sym kontinue
  | Ast.UnOp (_, op, prec, r) -> annotate_un_op env op prec r kontinue
  | Ast.BinOp (_, op, prec, l, r) -> annotate_bin_op env op prec l r kontinue
  | Ast.Slice (_, expr, start, stop) -> annotate_slice env expr start stop kontinue
  | Ast.Index (_, expr, idx) -> annotate_index env expr idx kontinue
  | Ast.If (_, c, t, f) -> annotate_cond env c t f kontinue
  | Ast.Case (_, scrut, clauses) -> annotate_case env scrut clauses kontinue
  | Ast.Let (_, (_, patt, ty, expr), rest) -> annotate_bind env patt ty expr rest kontinue
  | Ast.LetRec (_, bs, rest) -> annotate_bind_rec env bs rest kontinue
  | Ast.Abs (_, patt, ty, res, expr) -> annotate_abs env patt ty res expr kontinue
  | Ast.App (_, f, x) -> annotate_app env f x kontinue

and annotate_unit kontinue =
  unit
    |> kontinue Type.unit

and annotate_bool b kontinue =
  bool b
    |> kontinue Type.bool

and annotate_int i kontinue =
  int i
    |> kontinue Type.int

and annotate_float f kontinue =
  float f
    |> kontinue Type.float

and annotate_string len s kontinue =
  string len s
    |> kontinue Type.string

and annotate_blob len bs kontinue =
  blob len bs
    |> kontinue Type.blob

and annotate_timestamp ts kontinue =
  timestamp ts
    |> kontinue Type.timestamp

and annotate_duration d kontinue =
  duration d
    |> kontinue Type.duration

and annotate_tuple env exprs kontinue =

and annotate_record env constr fields kontinue =

and annotate_var env sym kontinue =
  try
    let ty = Type.lookup sym env in
    var sym
      |> kontinue ty
  with Not_found -> Type.unbound_identifier sym

and annotate_un_op env op r kontinue =
  annotate_expr env r (fun ty r ->
    let ty = Op.type_of_un op ty in
    un_op op r
      |> kontinue ty)

and annotate_bin_op env l op r kontinue =
  annotate_expr env l (fun l_ty l ->
    annotate_expr env r (fun r_ty r ->
      let (ty, expr) = match l_ty, op, r_ty with
        | Type.Bool, Op.And, Type.Bool ->
          case l [
            clause (Patt.bool false) (bool false);
            clause Patt.ground r;
          ] Type.bool
        | Type.Bool, Op.Or, Type.Bool ->
          case l [
            clause (Patt.bool true) (bool true);
            clause Patt.ground r;
          ] Type.bool
        | _ ->
          match l_ty, op, r_ty with
            | Type.Int, Op.Add, Type.Int ->
            | Type.Int, Op.Sub, Type.Int ->
            | Type.Int, Op.Mul, Type.Int ->
            | Type.Int, Op.Div, Type.Int ->
            | Type.Int, Op.Mod, Type.Int ->
            | Type.Int, Op.Eq, Type.Int ->
            | Type.Bool, Op.Eq, Type.Bool ->
            | Type.Int, Op.Neq, Type.Int ->
            | Type.Bool, Op.Neq, Type.Bool ->
            | Type.Int, Op.Lte, Type.Int ->
            | Type.Int, Op.Lt, Type.Int ->
            | Type.Int, Op.Gt, Type.Int ->
            | Type.Int, Op.Gte, Type.Int ->
          in
          builtin fn [l; r]
      in
      let ty = Op.type_of_bin l_ty op r_ty in
      kontinue ty expr))

and annotate_cond env c t f kontinue =
  annotate_expr env c (fun c_ty c ->
    match c_ty with
      | Type.Bool ->
        annotate_expr env t (fun t_ty t ->
          annotate_expr env f (fun f_ty f ->
            if Type.equal t_ty f_ty
            then
              case c [
                clause (Patt.bool true)  t;
                clause (Patt.bool false) f]
                t_ty
                |> kontinue t_ty
            else Type.conditional_branch_mismatch t_ty f_ty))
      | ty -> Type.invalid_condition ty)

(* and annotate_case env scrut clauses kontinue = *)
and annotate_case _ _ _ _ = failwith "TODO"

and annotate_bind env patt ty expr rest kontinue =
  annotate_binding env patt ty expr (fun env b ->
    annotate_expr env rest (fun rest_ty rest ->
      bind b rest
        |> kontinue rest_ty))

and annotate_binding env patt ty expr kontinue =
  annotate_expr env expr (fun expr_ty expr ->
    let env = match ty with
      | Some ty ->
        if Type.equal expr_ty ty
        then Type.bind patt expr_ty env
        else Type.declaration_mismatch patt ty expr_ty
      | None -> Type.bind patt expr_ty env
    in
    binding patt expr_ty expr
      |> kontinue env)

and annotate_bind_rec env bs rest kontinue =
  let env = bindings_env env bs in
  annotate_bindings env bs (fun bs ->
    annotate_expr env rest (fun rest_ty rest ->
      bind_rec bs rest
        |> kontinue rest_ty))

and annotate_bindings env bs kontinue = match bs with
  | [] -> kontinue []
  | (_, patt, ty, expr) :: bs ->
    annotate_expr env expr (fun expr_ty expr ->
      match ty with
        | Some ty ->
          if Type.equal expr_ty ty
          then
            annotate_bindings env bs (fun bs ->
              binding patt expr_ty expr :: bs
                |> kontinue)
          else Type.declaration_mismatch patt ty expr_ty
        | None -> Type.annotation_required patt)

and annotate_abs env patt ty res expr kontinue =
  let env = Type.bind patt ty env in
  annotate_expr env expr (fun expr_ty expr ->
    let res = match res with
      | Some res ->
        if Type.equal res expr_ty
        then expr_ty
        else Type.result_mismatch res expr_ty
      | None -> expr_ty
    in
    abs patt ty res expr
      |> kontinue (Type.func ty res))

and annotate_app env f x kontinue =
  annotate_expr env f (fun f_ty f ->
    match f_ty with
      | Type.Fun (arg, res) ->
          annotate_expr env x (fun x_ty x ->
            if Type.equal arg x_ty
            then
              app f x
                |> kontinue res
            else Type.invalid_args arg x_ty)
      | _ -> Type.cannot_apply f_ty)

let annotate_top_bind env patt ty expr kontinue =
  annotate_binding env patt ty expr (fun env b ->
    top_bind b
      |> kontinue env)

let annotate_top_bind_rec env bs kontinue =
  let env = bindings_env env bs in
  annotate_bindings env bs (fun bs ->
    top_bind_rec bs
      |> kontinue env)

let annotate_top env top kontinue = match top with
  | Ast.TopLet (_, (_, patt, ty, expr)) -> annotate_top_bind env patt ty expr kontinue
  | Ast.TopRec (_, bs) -> annotate_top_bind_rec env bs kontinue

let rec annotate_file env file kontinue = match file with
  | [] -> []
  | top :: file ->
    annotate_top env top (fun env top ->
      annotate_file env file (fun env file ->
        top :: file
          |> kontinue env))

let annotate_file env file = annotate_file env file (fun env file -> (env, file))
let annotate_top env top = annotate_top env top (fun env top -> (env, top))
let annotate_expr env expr = annotate_expr env expr (fun _ expr -> expr)

let annotate env files = (env, unit)

(* Operations *)

let precedence = function
  | Unit | Bool _ | Int _ | Float _ | String _ | Var _ -> 0
  | App _ | Builtin _ -> 1
  | Case _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15
