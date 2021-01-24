open Format
open OUnit2
open Nile.Syntax

let assert_lexes ~ctxt expected str =
  let len = String.length str in
  match expected, Lexer.from_string str |> Lexer.lex with
    | Parser.EOF _, Parser.EOF loc
    | Parser.LPAREN _, Parser.LPAREN loc
    | Parser.RPAREN _, Parser.RPAREN loc
    | Parser.COLON _, Parser.COLON loc
    | Parser.ARROW _, Parser.ARROW loc
    | Parser.DARROW _, Parser.DARROW loc
    | Parser.BIND _, Parser.BIND loc
    | Parser.COMMA _, Parser.COMMA loc
    | Parser.LET _, Parser.LET loc
    | Parser.REC _, Parser.REC loc
    | Parser.AND _, Parser.AND loc
    | Parser.IN _, Parser.IN loc
    | Parser.IF _, Parser.IF loc
    | Parser.THEN _, Parser.THEN loc
    | Parser.ELSE _, Parser.ELSE loc
    | Parser.ADD _, Parser.ADD loc
    | Parser.SUB _, Parser.SUB loc
    | Parser.MUL _, Parser.MUL loc
    | Parser.DIV _, Parser.DIV loc
    | Parser.MOD _, Parser.MOD loc
    | Parser.LAND _, Parser.LAND loc
    | Parser.LOR _, Parser.LOR loc
    | Parser.LNOT _, Parser.LNOT loc
    | Parser.EQ _, Parser.EQ loc
    | Parser.NEQ _, Parser.NEQ loc
    | Parser.LTE _, Parser.LTE loc
    | Parser.LT _, Parser.LT loc
    | Parser.GT _, Parser.GT loc
    | Parser.GTE _, Parser.GTE loc -> LocTest.assert_loc ~ctxt "-" (1, 0, 0) (1, len, len) len loc
    | Parser.BOOL (_, b), Parser.BOOL (loc, b') ->
      LocTest.assert_loc ~ctxt "-" (1, 0, 0) (1, len, len) len loc;
      assert_equal ~ctxt ~printer:string_of_bool b b' ~msg:"Token values are not equal"
    | Parser.INT (_, i), Parser.INT (loc, i') ->
      LocTest.assert_loc ~ctxt "-" (1, 0, 0) (1, len, len) len loc;
      assert_equal ~ctxt ~printer:string_of_int i i' ~msg:"Token values are not equal"
    | Parser.UIDENT (_, id), Parser.UIDENT (loc, id')
    | Parser.LIDENT (_, id), Parser.LIDENT (loc, id') ->
      LocTest.assert_loc ~ctxt "-" (1, 0, 0) (1, len, len) len loc;
      assert_equal ~ctxt ~printer:Fun.id id id' ~msg:"Token values are not equal"
    | _ ->
      assert_failure "Mismatched tokens"

(* Constructors *)

let code = "->"

let assert_lexes_code ~ctxt fname lexbuf =
  match Lexer.lex lexbuf with
    | Parser.ARROW loc -> LocTest.assert_loc ~ctxt fname (1, 0, 0) (1, 2, 2) 2 loc
    | _ -> assert_failure "Mismatched tokens"

let test_from_string ctxt =
  code
    |> Lexer.from_string
    |> assert_lexes_code ~ctxt "-"

let test_from_channel ctxt =
  let name = "different-file-name" in

  let (fname, oc) = Filename.open_temp_file "" "" in
  output_string oc code;
  close_out oc;

  let ic = open_in fname in
  let finally _ = close_in ic in
  let fn _ =
    ic
      |> Lexer.from_channel ~fname:name
      |> assert_lexes_code ~ctxt name
  in
  Fun.protect ~finally fn;

  let ic = open_in fname in
  let finally _ = close_in ic in
  let fn _ =
    ic
      |> Lexer.from_channel
      |> assert_lexes_code ~ctxt ""
  in
  Fun.protect ~finally fn

let test_from_file ctxt =
  let (fname, oc) = Filename.open_temp_file "" "" in
  output_string oc code;
  close_out oc;

  fname
    |> Lexer.from_file
    |> assert_lexes_code ~ctxt fname

let test_constructor =
  "Constructors" >::: [
    "From String"        >:: test_from_string;
    "From Input Channel" >:: test_from_channel;
    "From File"          >:: test_from_file;
  ]

(* Tokens *)

let test_eof ctxt =
  ""
    |> assert_lexes ~ctxt (Parser.EOF LocTest.dummy)

let test_newline ctxt =
  let expected = "foo" in
  let str = sprintf "   \n  %s\n  " expected in
  match Lexer.from_string str |> Lexer.lex with
    | Parser.LIDENT (loc, id) ->
      LocTest.assert_loc ~ctxt "-" (2, 2, 6) (2, 5, 9) 3 loc;
      assert_equal ~ctxt ~printer:Fun.id expected id ~msg:"Token values are not equal"
    | _ -> assert_failure "Mismatched tokens"

let test_punc_lparen ctxt =
  "("
    |> assert_lexes ~ctxt (Parser.LPAREN LocTest.dummy)

let test_punc_rparen ctxt =
  ")"
    |> assert_lexes ~ctxt (Parser.RPAREN LocTest.dummy)

let test_punc_colon ctxt =
  ":"
    |> assert_lexes ~ctxt (Parser.COLON LocTest.dummy)

let test_punc_arrow ctxt =
  "->"
    |> assert_lexes ~ctxt (Parser.ARROW LocTest.dummy)

let test_punc_darrow ctxt =
  "=>"
    |> assert_lexes ~ctxt (Parser.DARROW LocTest.dummy)

let test_punc_bind ctxt =
  "="
    |> assert_lexes ~ctxt (Parser.BIND LocTest.dummy)

let test_punc_comma ctxt =
  ","
    |> assert_lexes ~ctxt (Parser.COMMA LocTest.dummy)

let test_kwd_let ctxt =
  "let"
    |> assert_lexes ~ctxt (Parser.LET LocTest.dummy)

let test_kwd_rec ctxt =
  "rec"
    |> assert_lexes ~ctxt (Parser.REC LocTest.dummy)

let test_kwd_and ctxt =
  "and"
    |> assert_lexes ~ctxt (Parser.AND LocTest.dummy)

let test_kwd_in ctxt =
  "in"
    |> assert_lexes ~ctxt (Parser.IN LocTest.dummy)

let test_kwd_if ctxt =
  "if"
    |> assert_lexes ~ctxt (Parser.IF LocTest.dummy)

let test_kwd_then ctxt =
  "then"
    |> assert_lexes ~ctxt (Parser.THEN LocTest.dummy)

let test_kwd_else ctxt =
  "else"
    |> assert_lexes ~ctxt (Parser.ELSE LocTest.dummy)

let test_op_add ctxt =
  "+"
    |> assert_lexes ~ctxt (Parser.ADD LocTest.dummy)

let test_op_sub ctxt =
  "-"
    |> assert_lexes ~ctxt (Parser.SUB LocTest.dummy)

let test_op_mul ctxt =
  "*"
    |> assert_lexes ~ctxt (Parser.MUL LocTest.dummy)

let test_op_div ctxt =
  "/"
    |> assert_lexes ~ctxt (Parser.DIV LocTest.dummy)

let test_op_mod ctxt =
  "%"
    |> assert_lexes ~ctxt (Parser.MOD LocTest.dummy)

let test_op_and ctxt =
  "&&"
    |> assert_lexes ~ctxt (Parser.LAND LocTest.dummy)

let test_op_or ctxt =
  "||"
    |> assert_lexes ~ctxt (Parser.LOR LocTest.dummy)

let test_op_not ctxt =
  "!"
    |> assert_lexes ~ctxt (Parser.LNOT LocTest.dummy)

let test_op_eq ctxt =
  "=="
    |> assert_lexes ~ctxt (Parser.EQ LocTest.dummy)

let test_op_neq ctxt =
  "!="
    |> assert_lexes ~ctxt (Parser.NEQ LocTest.dummy)

let test_op_lte ctxt =
  "<="
    |> assert_lexes ~ctxt (Parser.LTE LocTest.dummy)

let test_op_lt ctxt =
  "<"
    |> assert_lexes ~ctxt (Parser.LT LocTest.dummy)

let test_op_gt ctxt =
  ">"
    |> assert_lexes ~ctxt (Parser.GT LocTest.dummy)

let test_op_gte ctxt =
  ">="
    |> assert_lexes ~ctxt (Parser.GTE LocTest.dummy)

let test_lit_bool ctxt =
  "true"
    |> assert_lexes ~ctxt (Parser.BOOL (LocTest.dummy, true));
  "false"
    |> assert_lexes ~ctxt (Parser.BOOL (LocTest.dummy, false))

let test_lit_int ctxt =
  "0"
    |> assert_lexes ~ctxt (Parser.INT (LocTest.dummy, 0));
  "42"
    |> assert_lexes ~ctxt (Parser.INT (LocTest.dummy, 42));
  "+42"
    |> assert_lexes ~ctxt (Parser.INT (LocTest.dummy, 42));
  "-42"
    |> assert_lexes ~ctxt (Parser.INT (LocTest.dummy, (-42)))

let test_uident ctxt =
  "Id"
    |> assert_lexes ~ctxt (Parser.UIDENT (LocTest.dummy, "Id"))

let test_lident ctxt =
  "id"
    |> assert_lexes ~ctxt (Parser.LIDENT (LocTest.dummy, "id"))

let test_tokens =
  "Tokens" >::: [
    "Non-Printable" >::: [
      "End-Of-File" >:: test_eof;
      "New Line"    >:: test_newline;
    ];
    "Punctuation" >::: [
      "Left Parenthesis"  >:: test_punc_lparen;
      "Right Parenthesis" >:: test_punc_rparen;
      "Colon"             >:: test_punc_colon;
      "Single Arrow"      >:: test_punc_arrow;
      "Double Arrow"      >:: test_punc_arrow;
      "Bind"              >:: test_punc_bind;
      "Comma"             >:: test_punc_comma;
    ];
    "Keywords" >::: [
      "Let"  >:: test_kwd_let;
      "Rec"  >:: test_kwd_rec;
      "And"  >:: test_kwd_and;
      "In"   >:: test_kwd_in;
      "If"   >:: test_kwd_if;
      "Then" >:: test_kwd_then;
      "Else" >:: test_kwd_else;
    ];
    "Operators" >::: [
      "Addition"              >:: test_op_add;
      "Subtraction"           >:: test_op_sub;
      "Multiplication"        >:: test_op_mul;
      "Integer Division"      >:: test_op_div;
      "Modulus"               >:: test_op_mod;
      "Logical And"           >:: test_op_and;
      "Logical Or"            >:: test_op_or;
      "Logical Not"           >:: test_op_not;
      "Equality"              >:: test_op_eq;
      "Inequality"            >:: test_op_neq;
      "Less Than or Equal"    >:: test_op_lte;
      "Less Than"             >:: test_op_lt;
      "Greater Than"          >:: test_op_gt;
      "Greater Than or Equal" >:: test_op_gte;
    ];
    "Literals" >::: [
      "Boolean" >:: test_lit_bool;
      "Integer" >:: test_lit_int;
    ];
    "Identifiers" >::: [
      "Upper Case" >:: test_uident;
      "Lower Case" >:: test_lident;
    ];
  ]

(* Suite *)

let suite =
  "Lexer" >::: [
    test_constructor;
    test_tokens;
  ]
