(* open Format *)
open OUnit2
(* open Ir *)

(*
let assert_un_equal ~ctxt expected actual = match expected, actual with
  | Anf.Not, Anf.Not -> let _ = ctxt in ()
  (* | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_un) ~msg:"Unary operators are not equal" expected actual *)

let assert_bin_equal ~ctxt expected actual = match expected, actual with
  | Anf.Add, Anf.Add
  | Anf.Sub, Anf.Sub
  | Anf.Mul, Anf.Mul
  | Anf.Div, Anf.Div
  | Anf.Mod, Anf.Mod
  | Anf.And, Anf.And
  | Anf.Or, Anf.Or
  | Anf.Eq, Anf.Eq
  | Anf.Neq, Anf.Neq
  | Anf.Lte, Anf.Lte
  | Anf.Lt, Anf.Lt
  | Anf.Gt, Anf.Gt
  | Anf.Gte, Anf.Gte -> ()
  | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_bin) ~msg:"Binary operators are not equal" expected actual

let assert_prim_equal ~ctxt expected actual = match expected, actual with
  | Anf.Bool b, Anf.Bool b' -> assert_equal ~ctxt ~printer:string_of_bool ~msg:"Boolean values are not equal" b b'
  | Anf.Int i, Anf.Int i' -> assert_equal ~ctxt ~printer:string_of_int ~msg:"Integer values are not equal" i i'
  | Anf.Var id, Anf.Var id' -> assert_equal ~ctxt ~printer:Fun.id ~msg:"Identifiers are not equal" id id'
  | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_prim) ~msg:"Primitive values are not equal" expected actual

let assert_stmt_equal ~ctxt expected actual = match expected, actual with
  | Anf.UnOp (op, r), Anf.UnOp (op', r') ->
    assert_un_equal ~ctxt op op';
    assert_prim_equal ~ctxt r r'
  | Anf.BinOp (l, op, r), Anf.BinOp (l', op', r') ->
    assert_bin_equal ~ctxt op op';
    assert_prim_equal ~ctxt l l';
    assert_prim_equal ~ctxt r r'
  | Anf.App (fn, args), Anf.App (fn', args') ->
    assert_equal ~ctxt ~printer:Fun.id ~msg:"Function names are not equal" fn fn';
    let iter (arg, arg') = assert_prim_equal ~ctxt arg arg' in
    args'
      |> List.combine args
      |> List.iter iter
  | Anf.Prim p, Anf.Prim p' -> assert_prim_equal ~ctxt p p'
  | expected, actual -> assert_equal ~ctxt ~cmp:never ~printer:(printer Anf.pp_stmt) ~msg:"Statments are not equal" expected actual

let assert_pp pp ~ctxt lines anf =
  let printer str =
    let map str = sprintf "%S" str in
    str
      |> String.split_on_char '\n'
      |> List.map map
      |> String.concat "\n"
  in
  let expected = String.concat "\n" lines in
  fprintf str_formatter "%t" (pp anf)
    |> flush_str_formatter
    |> assert_equal ~ctxt ~printer expected *)

let suite =
  "Administrative Normal Form" >::: [
  ]
