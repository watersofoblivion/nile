open Format
open Common

(* Symbolization *)

type sym = int

module StringMap = Map.Make (struct
  type t = string
  let compare = compare
end)

module SymMap = Map.Make (struct
  type t = sym
  let compare = compare
end)

type tbl = {
  idx:   sym;
  syms:  sym StringMap.t;
  names: string SymMap.t
}

let tbl =
  { idx   = 0;
    syms  = StringMap.empty;
    names = SymMap.empty }

let symbolize str tbl =
  try (StringMap.find str tbl.syms, tbl)
  with Not_found ->
    let tbl' =
      { idx   = tbl.idx + 1;
        syms  = StringMap.add str tbl.idx tbl.syms;
        names = SymMap.add tbl.idx str tbl.names }
    in
    (tbl.idx, tbl')

let name_of sym tbl = SymMap.find sym tbl.names

(* Syntax *)

type expr =
  | Bool of Loc.t * bool
  | Int of Loc.t * int
  | Var of Loc.t * sym
  | UnOp of Loc.t * Op.un * expr
  | BinOp of Loc.t * expr * Op.bin * expr
  | If of Loc.t * expr * expr * expr
  | Let of Loc.t * binding * expr
  | LetRec of Loc.t * binding list * expr
  | Abs of Loc.t * sym * Type.t * Type.t option * expr
  | App of Loc.t * expr * expr
and binding = Loc.t * sym * Type.t option * expr

type top =
  | TopLet of Loc.t * binding
  | TopRec of Loc.t * binding list

type file = top list

(* Constructors *)

let bool loc b = Bool (loc, b)
let int loc i = Int (loc, i)
let var loc id = Var (loc, id)
let un_op loc op r = UnOp (loc, op, r)
let bin_op loc l op r = BinOp (loc, l, op, r)
let bind loc b rest = Let (loc, b, rest)
let bind_rec loc bs rest = LetRec (loc, bs, rest)
let abs loc id ty res expr = Abs (loc, id, ty, res, expr)
let app loc f xs = App (loc, f, xs)
let cond loc c t f = If (loc, c, t, f)
let binding loc id ty expr = (loc, id, ty, expr)

let top_bind loc b = TopLet (loc, b)
let top_bind_rec loc bs = TopRec (loc, bs)

let file tops = tops

(* Operations *)

let precedence = function
  | Bool _ | Int _ | Var _ -> 0
  | App _ -> 1
  | UnOp (_, op, _) -> Op.un_precedence op
  | BinOp (_, _, op, _) -> Op.bin_precedence op
  | If _ -> 13
  | Abs _ -> 14
  | Let _ | LetRec _ -> 15

let loc_expr = function
  | Bool (loc, _)
  | Int (loc, _)
  | UnOp (loc, _, _)
  | BinOp (loc, _, _, _)
  | Let (loc, _, _)
  | LetRec (loc, _, _)
  | Abs (loc, _, _, _, _)
  | App (loc, _, _)
  | Var (loc, _)
  | If (loc, _, _, _) -> loc

let loc_top = function
  | TopLet (loc, _)
  | TopRec (loc, _) -> loc

(* Pretty Printing *)

let rec pp_expr tbl expr fmt = match expr with
  | Bool (_, b) -> pp_bool b fmt
  | Int (_, i) -> pp_int i fmt
  | Var (_, id) -> pp_var tbl id fmt
  | UnOp (_, op, r) -> pp_un_op tbl op r fmt
  | BinOp (_, l, op, r) -> pp_bin_op tbl l op r fmt
  | Let (_, b, rest) -> pp_bind tbl b rest fmt
  | LetRec (_, bs, rest) -> pp_bind_rec tbl bs rest fmt
  | Abs (_, id, ty, res, expr) -> pp_abs tbl id ty res expr fmt
  | App (_, f, x) -> pp_app tbl f x fmt
  | If (_, c, t, f) -> pp_cond tbl c t f fmt

and pp_bool b fmt =
  fprintf fmt "%b" b

and pp_int i fmt =
  fprintf fmt "%d" i

and pp_var tbl id fmt =
  name_of id tbl
    |> fprintf fmt "%s"

and pp_un_op tbl op r fmt =
  let prec = Op.un_precedence op in
  fprintf fmt "%t%t" (Op.pp_un op) (print_precedence tbl prec r);

and pp_bin_op tbl l op r fmt =
  let prec = Op.bin_precedence op in
  fprintf fmt "@[<hov 2>%t@ %t@ %t@]" (print_precedence tbl prec l) (Op.pp_bin op) (print_precedence tbl prec r)

and pp_bind tbl b rest fmt =
  fprintf fmt "@[<v>@[<hv>let %t@ in@]@ %t@]" (pp_binding tbl b) (pp_expr tbl rest)

and pp_bind_rec tbl bs rest fmt =
  fprintf fmt "@[<v>@[<hv>@[<hv>let rec %t@ in@]@ %t@]" (pp_bindings tbl bs) (pp_expr tbl rest)

and pp_abs tbl id ty res expr fmt =
  let id = name_of id tbl in
  fprintf fmt "(%s: %t" id (Type.pp ty);
  let res = pp_params res expr fmt in
  let _ = match res with
    | Some res -> fprintf fmt "): %t" (Type.pp res)
    | None -> fprintf fmt ")"
  in
  fprintf fmt " => %t" (pp_expr tbl expr)

and pp_app tbl f x fmt =
  fprintf fmt "@[<hov 2>%t@ %t@]" (print_precedence tbl 0 f) (pp_expr tbl x)

and pp_cond tbl c t f fmt =
  fprintf fmt "@[<hv>@[<hv>if@;<1 2>%t@]@ @[<hv>then@;<1 2>%t@]@ @[<hv>else@;<1 2>%t@]@]" (pp_expr tbl c) (pp_expr tbl t) (pp_expr tbl f)

and print_precedence tbl prec expr fmt =
  if prec < precedence expr
  then fprintf fmt "(%t)" (pp_expr tbl expr)
  else fprintf fmt "%t" (pp_expr tbl expr)

and pp_bindings tbl bs fmt =
  let pp_sep fmt _ = fprintf fmt "@ @[<hv>and " in
  let pp_b fmt b = fprintf fmt "%t@]" (pp_binding tbl b) in
  pp_print_list ~pp_sep pp_b fmt bs

and pp_binding tbl (_, id, ty, expr) fmt =
  let id = name_of id tbl in
  match expr with
    | Abs (_, id', ty, res, expr) ->
      let id' = name_of id' tbl in
      fprintf fmt "%s(%s: %t" id id' (Type.pp ty);
      let res = pp_params res expr fmt in
      fprintf fmt ")";
      let _ = match res with
        | Some res -> fprintf fmt ": %t" (Type.pp res)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr tbl expr)
    | expr ->
      fprintf fmt "%s" id;
      let _ = match ty with
        | Some ty -> fprintf fmt ": %t" (Type.pp ty)
        | None -> ()
      in
      fprintf fmt " =@;<1 2>%t" (pp_expr tbl expr)

and pp_params tbl res expr fmt = match expr with
  | Abs (_, id, ty, res, expr) ->
    let id = name_of id tbl in
    fprintf fmt ", %s: %t" id (Type.pp ty);
    pp_params tbl res expr fmt
  | _ -> res

let pp_top_bind tbl b fmt =
  fprintf fmt "@[<hv>let %t@]" (pp_binding tbl b)

let pp_top_bind_rec tbl bs fmt =
  fprintf fmt "@[<hv>let rec %t" (pp_bindings tbl bs)

let pp_top tbl top fmt = match top with
  | TopLet (_, b) -> pp_top_bind tbl b fmt
  | TopRec (_, bs) -> pp_top_bind_rec tbl bs fmt

let pp_file tbl file fmt =
  let pp_sep fmt _ = fprintf fmt "@ @ " in
  let pp_top fmt top = pp_top tbl top fmt in
  fprintf fmt "@[<v>";
  pp_print_list ~pp_sep pp_top fmt file;
  fprintf fmt "@]"
