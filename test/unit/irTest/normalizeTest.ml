open OUnit2
open Nile.Common
open Nile.Syntax
open Nile.Ir
open CommonTest
open SyntaxTest

let assert_normalize ~ctxt ?alpha:(alpha = 0) ?aenv:(aenv = []) ?tenv:(tenv = []) idx ty expected ast =
Format.printf "Source AST:\n\n%t\n" (Syntax.Pretty.expr ast);
Format.printf "Expected ANF:\n\n%t\n" (Ir.Pretty.block expected);
  let (idx', ty', actual) = Normalize.ast alpha aenv tenv None ast in
  try
    AnfTest.assert_block_equal ~ctxt expected actual;
    TypeTest.assert_type_equal ~ctxt ty ty';
    assert_equal ~ctxt ~printer:string_of_int ~msg:"Alpha indexes are not equal" idx idx'
  with exn ->
    Format.printf "Source AST:\n\n%t\n" (Syntax.Pretty.expr ast);
    Format.printf "Expected ANF:\n\n%t\n" (Ir.Pretty.block expected);
    Format.printf "Actual ANF:\n\n%t\n" (Ir.Pretty.block actual);
    raise exn

let bin_op constr l op r = Anf.bin_op (constr l) op (constr r)
let bind_bin_op constr idx ty l op r =
  bin_op constr l op r
    |> Anf.binding idx ty

let int_bin_op = bin_op Anf.int
let bind_int_bin_op = bind_bin_op Anf.int

let var_bin_op = bin_op Anf.var
let bind_var_bin_op = bind_bin_op Anf.var

let src_int_bin_op l op r =
  let l = Ast.int LocTest.dummy l in
  let r = Ast.int LocTest.dummy r in
  Ast.bin_op LocTest.dummy l op r

let suite =
  let test_int =
    let test_int ctxt =
      let expected =
        Anf.int 1
          |> Anf.atom
          |> Anf.expr
      in

      Ast.int LocTest.dummy 1
        |> assert_normalize ~ctxt 0 Type.int expected
    in
    "Integer" >:: test_int
  in
  let test_bool =
    let test_bool ctxt =
      let expected =
        Anf.bool true
          |> Anf.atom
          |> Anf.expr
      in

      Ast.bool LocTest.dummy true
        |> assert_normalize ~ctxt 0 Type.bool expected
    in
    "Boolean" >:: test_bool
  in
  let test_var =
    let test_var ctxt =
      let id = "x" in
      let idx = 0 in
      let ty = Type.bool in
      let aenv = [(id, idx)] in
      let tenv = [(idx, ty)] in

      let expected =
        Anf.var idx
          |> Anf.atom
          |> Anf.expr
      in

      Ast.var LocTest.dummy id
        |> assert_normalize ~ctxt ~aenv ~tenv 0 ty expected
    in
    "Variable" >:: test_var
  in
  let test_un_op =
    let test_simple ctxt =
      let expected =
        Anf.bool true
          |> Anf.un_op Op.un_not
          |> Anf.expr
      in

      Ast.bool LocTest.dummy true
        |> Ast.un_op LocTest.dummy Op.un_not
        |> assert_normalize ~ctxt 0 Type.bool expected
    in
    let test_complex ctxt =
      let expected =
        let zero = bind_int_bin_op 0 Type.bool 1 Op.bin_eq 2 in
        Anf.var 0
          |> Anf.un_op Op.un_not
          |> Anf.expr
          |> Anf.bind zero
      in

      src_int_bin_op 1 Op.bin_eq 2
        |> Ast.un_op LocTest.dummy Op.un_not
        |> assert_normalize ~ctxt 1 Type.bool expected
    in
    let test_nested ctxt =
      let expected =
        let zero =
          Anf.bool true
            |> Anf.un_op Op.un_not
            |> Anf.binding 0 Type.bool
        in
        Anf.var 0
          |> Anf.un_op Op.un_not
          |> Anf.expr
          |> Anf.bind zero
      in

      Ast.bool LocTest.dummy true
        |> Ast.un_op LocTest.dummy Op.un_not
        |> Ast.un_op LocTest.dummy Op.un_not
        |> assert_normalize ~ctxt 1 Type.bool expected
    in
    "Unary Operator" >::: [
      "Simple"  >:: test_simple;
      "Complex" >:: test_complex;
      "Nested"  >:: test_nested;
    ]
  in
  let test_bin_op =
    let test_simple ctxt =
      let expected =
        int_bin_op 1 Op.bin_eq 2
          |> Anf.expr
      in

      src_int_bin_op 1 Op.bin_eq 2
        |> assert_normalize ~ctxt 0 Type.bool expected
    in
    let test_complex ctxt =
      let expected =
        let zero = bind_int_bin_op 0 Type.int 1 Op.bin_add 2 in
        let one = bind_int_bin_op 1 Type.int 3 Op.bin_sub 4 in
        let two = bind_var_bin_op 2 Type.bool 0 Op.bin_eq 1 in
        let three = bind_int_bin_op 3 Type.int 5 Op.bin_mul 6 in
        let four = bind_int_bin_op 4 Type.int 7 Op.bin_div 8 in
        let five = bind_var_bin_op 5 Type.bool 3 Op.bin_neq 4 in
        var_bin_op 2 Op.bin_or 5
          |> Anf.expr
          |> Anf.bind five
          |> Anf.bind four
          |> Anf.bind three
          |> Anf.bind two
          |> Anf.bind one
          |> Anf.bind zero
      in

      let l =
        let l = src_int_bin_op 1 Op.bin_add 2 in
        let r = src_int_bin_op 3 Op.bin_sub 4 in
        Ast.bin_op LocTest.dummy l Op.bin_eq r
      in
      let r =
        let l = src_int_bin_op 5 Op.bin_mul 6 in
        let r = src_int_bin_op 7 Op.bin_div 8 in
        Ast.bin_op LocTest.dummy l Op.bin_neq r
      in
      Ast.bin_op LocTest.dummy l Op.bin_or r
        |> assert_normalize ~ctxt 6 Type.bool expected
    in
    "Binary Operator" >::: [
      "Simple" >:: test_simple;
      "Complex" >:: test_complex;
    ]
  in
  let test_cond =
    let test_simple ctxt =
      let expected =
        let c = Anf.bool true in
        let t =
          Anf.int 1
            |> Anf.atom
            |> Anf.expr
        in
        let f =
          Anf.int 2
            |> Anf.atom
            |> Anf.expr
        in
        Anf.cond c t f
      in

      let c = Ast.bool LocTest.dummy true in
      let t = Ast.int LocTest.dummy 1 in
      let f = Ast.int LocTest.dummy 2 in
      Ast.cond LocTest.dummy c t f
        |> assert_normalize ~ctxt 0 Type.int expected
    in
    let test_complex ctxt =
      let expected =
        let zero = bind_int_bin_op 0 Type.bool 1 Op.bin_eq 2 in
        let one = bind_int_bin_op 1 Type.bool 3 Op.bin_neq 4 in
        let two = bind_var_bin_op 2 Type.bool 0 Op.bin_or 1 in
        let t =
          let three = bind_int_bin_op 3 Type.int 5 Op.bin_add 6 in
          let four = bind_int_bin_op 4 Type.int 7 Op.bin_sub 8 in
          var_bin_op 3 Op.bin_mul 4
            |> Anf.expr
            |> Anf.bind four
            |> Anf.bind three
        in
        let f =
          let five = bind_int_bin_op 5 Type.int 9 Op.bin_div 10 in
          let six = bind_int_bin_op 6 Type.int 11 Op.bin_mod 12 in
          var_bin_op 5 Op.bin_add 6
            |> Anf.expr
            |> Anf.bind six
            |> Anf.bind five
        in
        Anf.cond (Anf.var 2) t f
          |> Anf.bind two
          |> Anf.bind one
          |> Anf.bind zero
      in

      let c =
        let l = src_int_bin_op 1 Op.bin_eq 2 in
        let r = src_int_bin_op 3 Op.bin_neq 4 in
        Ast.bin_op LocTest.dummy l Op.bin_or r
      in
      let t =
        let l = src_int_bin_op 5 Op.bin_add 6 in
        let r = src_int_bin_op 7 Op.bin_sub 8 in
        Ast.bin_op LocTest.dummy l Op.bin_mul r
      in
      let f =
        let l = src_int_bin_op 9 Op.bin_div 10 in
        let r = src_int_bin_op 11 Op.bin_mod 12 in
        Ast.bin_op LocTest.dummy l Op.bin_add r
      in
      Ast.cond LocTest.dummy c t f
        |> assert_normalize ~ctxt 7 Type.int expected
    in
    let test_nested ctxt =
      let expected =
        let nested' =
          let t =
            let zero = bind_int_bin_op 0 Type.int 21 Op.bin_div 22 in
            let one = bind_int_bin_op 1 Type.int 23 Op.bin_mod 24 in
            Anf.bin_op (Anf.var 0) Op.bin_add (Anf.var 1)
              |> Anf.expr
              |> Anf.bind one
              |> Anf.bind zero
          in
          let f =
            let two = bind_int_bin_op 2 Type.int 25 Op.bin_sub 26 in
            let three = bind_int_bin_op 3 Type.int 27 Op.bin_mul 28 in
            Anf.bin_op (Anf.var 2) Op.bin_div (Anf.var 3)
              |> Anf.expr
              |> Anf.bind three
              |> Anf.bind two
          in
          Anf.cond (Anf.var 4) t f
            |> Anf.abs 4 Type.bool Type.int
            |> Anf.atom
            |> Anf.binding 5 (Type.func Type.bool Type.int)
        in
        let nested =
          let t =
            let six = bind_int_bin_op 6 Type.int 13 Op.bin_mod 14 in
            let seven = bind_int_bin_op 7 Type.int 15 Op.bin_add 16 in
            let eight = bind_var_bin_op 8 Type.bool 6 Op.bin_gte 7 in
            Anf.app (Anf.var 5) (Anf.var 8)
              |> Anf.expr
              |> Anf.bind eight
              |> Anf.bind seven
              |> Anf.bind six
          in
          let f =
            let nine = bind_int_bin_op 9 Type.int 17 Op.bin_sub 18 in
            let ten = bind_int_bin_op 10 Type.int 19 Op.bin_mul 20 in
            let eleven = bind_var_bin_op 11 Type.bool 9 Op.bin_gt 10 in
            Anf.app (Anf.var 5) (Anf.var 11)
              |> Anf.expr
              |> Anf.bind eleven
              |> Anf.bind ten
              |> Anf.bind nine
          in
          Anf.cond (Anf.var 12) t f
            |> Anf.abs 12 Type.bool Type.int
            |> Anf.atom
            |> Anf.binding 13 (Type.func Type.bool Type.int)
        in
        let t =
          let seventeen = bind_int_bin_op 17 Type.int 5 Op.bin_add 6 in
          let eighteen = bind_int_bin_op 18 Type.int 7 Op.bin_sub 8 in
          let nineteen = bind_var_bin_op 19 Type.bool 17 Op.bin_lt 18 in
          Anf.app (Anf.var 13) (Anf.var 19)
            |> Anf.expr
            |> Anf.bind nineteen
            |> Anf.bind eighteen
            |> Anf.bind seventeen
        in
        let f =
          let twenty = bind_int_bin_op 20 Type.int 9 Op.bin_mul 10 in
          let twenty_one = bind_int_bin_op 21 Type.int 11 Op.bin_div 12 in
          let twenty_two = bind_var_bin_op 22 Type.bool 20 Op.bin_lte 21 in
          Anf.app (Anf.var 13) (Anf.var 22)
            |> Anf.expr
            |> Anf.bind twenty_two
            |> Anf.bind twenty_one
            |> Anf.bind twenty
        in
        let fourteen = bind_int_bin_op 14 Type.bool 1 Op.bin_eq 2 in
        let fifteen = bind_int_bin_op 15 Type.bool 3 Op.bin_neq 4 in
        let sixteen = bind_var_bin_op 16 Type.bool 14 Op.bin_or 15 in
        Anf.cond (Anf.var 16) t f
          |> Anf.bind sixteen
          |> Anf.bind fifteen
          |> Anf.bind fourteen
          |> Anf.bind nested
          |> Anf.bind nested'
      in

      let nested =
        let nested' =
          let c =
            let l = src_int_bin_op 1 Op.bin_eq 2 in
            let r = src_int_bin_op 3 Op.bin_neq 4 in
            Ast.bin_op LocTest.dummy l Op.bin_or r
          in
          let t =
            let l = src_int_bin_op 5 Op.bin_add 6 in
            let r = src_int_bin_op 7 Op.bin_sub 8 in
            Ast.bin_op LocTest.dummy l Op.bin_lt r
          in
          let f =
            let l = src_int_bin_op 9 Op.bin_mul 10 in
            let r = src_int_bin_op 11 Op.bin_div 12 in
            Ast.bin_op LocTest.dummy l Op.bin_lte r
          in
          Ast.cond LocTest.dummy c t f
        in
        let t =
          let l = src_int_bin_op 13 Op.bin_mod 14 in
          let r = src_int_bin_op 15 Op.bin_add 16 in
          Ast.bin_op LocTest.dummy l Op.bin_gte r
        in
        let f =
          let l = src_int_bin_op 17 Op.bin_sub 18 in
          let r = src_int_bin_op 19 Op.bin_mul 20 in
          Ast.bin_op LocTest.dummy l Op.bin_gt r
        in
        Ast.cond LocTest.dummy nested' t f
      in
      let t =
        let l = src_int_bin_op 21 Op.bin_div 22 in
        let r = src_int_bin_op 23 Op.bin_mod 24 in
        Ast.bin_op LocTest.dummy l Op.bin_add r
      in
      let f =
        let l = src_int_bin_op 25 Op.bin_sub 26 in
        let r = src_int_bin_op 27 Op.bin_mul 28 in
        Ast.bin_op LocTest.dummy l Op.bin_div r
      in
      Ast.cond LocTest.dummy nested t f
        |> assert_normalize ~ctxt 23 Type.int expected
    in
    "Conditional" >::: [
      "Simple"  >:: test_simple;
      "Complex" >:: test_complex;
      "Nested"  >:: test_nested;
    ]
  in
  let test_bind =
    let test_simple ctxt =
      let expected =
        let zero = bind_int_bin_op 0 Type.int 1 Op.bin_add 2 in
        let one = bind_int_bin_op 1 Type.int 3 Op.bin_sub 4 in
        let two =
          Anf.bin_op (Anf.var 0) Op.bin_eq (Anf.var 1)
            |> Anf.binding 2 Type.bool
        in
        Anf.var 2
          |> Anf.atom
          |> Anf.expr
          |> Anf.bind two
          |> Anf.bind one
          |> Anf.bind zero
      in

      let l = src_int_bin_op 1 Op.bin_add 2 in
      let r = src_int_bin_op 3 Op.bin_sub 4 in
      let l_eq_r = Ast.bin_op LocTest.dummy l Op.bin_eq r in
      let id_x = "x" in
      let b = Ast.binding LocTest.dummy id_x Type.bool l_eq_r in
      Ast.var LocTest.dummy id_x
        |> Ast.bind LocTest.dummy b
        |> assert_normalize ~ctxt 3 Type.bool expected
    in
    let test_nested ctxt =
      let expected =
        let zero = bind_int_bin_op 0 Type.int 1 Op.bin_add 2 in
        let one = bind_int_bin_op 1 Type.int 3 Op.bin_sub 4 in
        let two = bind_var_bin_op 2 Type.int 0 Op.bin_mul 1 in
        let three = bind_int_bin_op 3 Type.int 5 Op.bin_div 6 in
        let four = bind_int_bin_op 4 Type.int 7 Op.bin_mod 8 in
        let five = bind_var_bin_op 5 Type.int 3 Op.bin_add 4 in
        let six = bind_var_bin_op 6 Type.int 2 Op.bin_sub 5 in
        let seven = bind_int_bin_op 7 Type.int 9 Op.bin_mul 10 in
        let eight = bind_int_bin_op 8 Type.int 11 Op.bin_div 12 in
        let nine = bind_var_bin_op 9 Type.int 7 Op.bin_mod 8 in
        let ten = bind_int_bin_op 10 Type.int 13 Op.bin_add 14 in
        let eleven = bind_int_bin_op 11 Type.int 15 Op.bin_sub 16 in
        let twelve = bind_var_bin_op 12 Type.int 10 Op.bin_mul 11 in
        let thirteen = bind_var_bin_op 13 Type.int 9 Op.bin_div 12 in

        Anf.bin_op (Anf.var 6) Op.bin_mod (Anf.var 13)
          |> Anf.expr
          |> Anf.bind thirteen
          |> Anf.bind twelve
          |> Anf.bind eleven
          |> Anf.bind ten
          |> Anf.bind nine
          |> Anf.bind eight
          |> Anf.bind seven
          |> Anf.bind six
          |> Anf.bind five
          |> Anf.bind four
          |> Anf.bind three
          |> Anf.bind two
          |> Anf.bind one
          |> Anf.bind zero
      in

      let id_l = "l" in
      let var_l = Ast.var LocTest.dummy id_l in

      let id_r = "r" in
      let var_r = Ast.var LocTest.dummy id_r in

      let l =
        let l =
          let l =
            src_int_bin_op 1 Op.bin_add 2
              |> Ast.binding LocTest.dummy id_l Type.int
          in
          let r =
            src_int_bin_op 3 Op.bin_sub 4
              |> Ast.binding LocTest.dummy id_r Type.int
          in
          Ast.bin_op LocTest.dummy var_l Op.bin_mul var_r
            |> Ast.bind LocTest.dummy r
            |> Ast.bind LocTest.dummy l
            |> Ast.binding LocTest.dummy id_l Type.int
        in
        let r =
          let l =
            src_int_bin_op 5 Op.bin_div 6
              |> Ast.binding LocTest.dummy id_l Type.int
          in
          let r =
            src_int_bin_op 7 Op.bin_mod 8
              |> Ast.binding LocTest.dummy id_r Type.int
          in
          Ast.bin_op LocTest.dummy var_l Op.bin_add var_r
            |> Ast.bind LocTest.dummy r
            |> Ast.bind LocTest.dummy l
            |> Ast.binding LocTest.dummy id_r Type.int
        in
        Ast.bin_op LocTest.dummy var_l Op.bin_sub var_r
          |> Ast.bind LocTest.dummy r
          |> Ast.bind LocTest.dummy l
          |> Ast.binding LocTest.dummy id_l Type.int
      in
      let r =
        let l =
          let l =
            src_int_bin_op 9 Op.bin_mul 10
              |> Ast.binding LocTest.dummy id_l Type.int
          in
          let r =
            src_int_bin_op 11 Op.bin_div 12
              |> Ast.binding LocTest.dummy id_r Type.int
          in
          Ast.bin_op LocTest.dummy var_l Op.bin_mod var_r
            |> Ast.bind LocTest.dummy r
            |> Ast.bind LocTest.dummy l
            |> Ast.binding LocTest.dummy id_l Type.int
        in
        let r =
          let l =
            src_int_bin_op 13 Op.bin_add 14
              |> Ast.binding LocTest.dummy id_l Type.int
          in
          let r =
            src_int_bin_op 15 Op.bin_sub 16
              |> Ast.binding LocTest.dummy id_r Type.int
          in
          Ast.bin_op LocTest.dummy var_l Op.bin_mul var_r
            |> Ast.bind LocTest.dummy r
            |> Ast.bind LocTest.dummy l
            |> Ast.binding LocTest.dummy id_r Type.int
        in
        Ast.bin_op LocTest.dummy var_l Op.bin_div var_r
          |> Ast.bind LocTest.dummy r
          |> Ast.bind LocTest.dummy l
          |> Ast.binding LocTest.dummy id_r Type.int
      in
      Ast.bin_op LocTest.dummy var_l Op.bin_mod var_r
        |> Ast.bind LocTest.dummy r
        |> Ast.bind LocTest.dummy l
        |> assert_normalize ~ctxt 14 Type.int expected
    in
    let test_cond ctxt =
      let expected =
        let three =
          let one =
            Anf.bin_op (Anf.var 0) Op.bin_div (Anf.int 14)
              |> Anf.binding 1 Type.int
          in
          let two =
            Anf.bin_op (Anf.var 0) Op.bin_mod (Anf.int 15)
              |> Anf.binding 2 Type.int
          in
          var_bin_op 1 Op.bin_add 2
            |> Anf.expr
            |> Anf.bind two
            |> Anf.bind one
            |> Anf.abs 0 Type.int Type.int
            |> Anf.atom
            |> Anf.binding 3 (Type.func Type.int Type.int)
        in
        let join = Anf.var 3 in

        let t =
          let seven = bind_int_bin_op 7 Type.int 1 Op.bin_add 2 in
          let eight = bind_int_bin_op 8 Type.int 3 Op.bin_sub 4 in
          let nine = bind_var_bin_op 9 Type.int 7 Op.bin_mul 8 in
          Anf.app join (Anf.var 9)
            |> Anf.expr
            |> Anf.bind nine
            |> Anf.bind eight
            |> Anf.bind seven
        in
        let f =
          let ten = bind_int_bin_op 10 Type.int 5 Op.bin_div 6 in
          let eleven = bind_int_bin_op 11 Type.int 7 Op.bin_mod 8 in
          let twelve = bind_var_bin_op 12 Type.int 10 Op.bin_add 11 in
          Anf.app join (Anf.var 12)
            |> Anf.expr
            |> Anf.bind twelve
            |> Anf.bind eleven
            |> Anf.bind ten
        in

        let four = bind_int_bin_op 4 Type.int 9 Op.bin_sub 10 in
        let five = bind_int_bin_op 5 Type.int 11 Op.bin_mul 12 in
        let six = bind_var_bin_op 6 Type.bool 4 Op.bin_eq 5 in
        Anf.cond (Anf.var 6) t f
          |> Anf.bind six
          |> Anf.bind five
          |> Anf.bind four
          |> Anf.bind three
      in

      let t =
        let l =
          src_int_bin_op 1 Op.bin_add 2
            |> Ast.binding LocTest.dummy "l" Type.int
        in
        let r =
          src_int_bin_op 3 Op.bin_sub 4
            |> Ast.binding LocTest.dummy "r" Type.int
        in
        Ast.bin_op LocTest.dummy (Ast.var LocTest.dummy "l") Op.bin_mul (Ast.var LocTest.dummy "r")
          |> Ast.bind LocTest.dummy r
          |> Ast.bind LocTest.dummy l
      in
      let f =
        let l =
          src_int_bin_op 5 Op.bin_div 6
            |> Ast.binding LocTest.dummy "l" Type.int
        in
        let r =
          src_int_bin_op 7 Op.bin_mod 8
            |> Ast.binding LocTest.dummy "r" Type.int
        in
        Ast.bin_op LocTest.dummy (Ast.var LocTest.dummy "l") Op.bin_add (Ast.var LocTest.dummy "r")
          |> Ast.bind LocTest.dummy r
          |> Ast.bind LocTest.dummy l
      in
      let cond =
        let l = src_int_bin_op 9 Op.bin_sub 10 in
        let r = src_int_bin_op 11 Op.bin_mul 12 in
        let c = Ast.bin_op LocTest.dummy l Op.bin_eq r in
        Ast.cond LocTest.dummy c t f
          |> Ast.binding LocTest.dummy "x" Type.int
      in
      let var_x = Ast.var LocTest.dummy "x" in
      let l = Ast.bin_op LocTest.dummy var_x Op.bin_div (Ast.int LocTest.dummy 13) in
      let r = Ast.bin_op LocTest.dummy var_x Op.bin_mod (Ast.int LocTest.dummy 14) in
      Ast.bin_op LocTest.dummy l Op.bin_add r
        |> Ast.bind LocTest.dummy cond
        |> assert_normalize ~ctxt 0 Type.int expected
    in
    "Value Binding" >::: [
      "Simple"      >:: test_simple;
      "Nested"      >:: test_nested;
      "Conditional" >:: test_cond;
    ]
  in


  let test_mixed =
    "Complex Mixed Flattening" >::: [

    ]
  in

  "Normalization" >::: [
    test_int;
    test_bool;
    test_var;
    test_un_op;
    test_bin_op;
    test_cond;
    test_bind;
    test_mixed;
  ]
