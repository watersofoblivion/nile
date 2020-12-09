open OUnit2
open Nile

let suite =
  let test_constructors =
    let code = "->" in

    let assert_lexes ~ctxt fname lexbuf =
      let tok = Lexer.lex lexbuf in

      let expected_loc = Loc.mock fname (1, 0, 0) (1, 2, 2) in
      let expected = Parser.ARROW Loc.dummy in
      tok
        |> Lexer.loc
        |> assert_equal ~ctxt ~cmp:Loc.equal expected_loc;
      tok
        |> Lexer.deloc
        |> assert_equal ~ctxt ~cmp:Lexer.equal expected
    in

    let test_from_string ctxt =
      code
        |> Lexer.from_string
        |> assert_lexes ~ctxt "-"
    in
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
          |> assert_lexes ~ctxt name
      in
      Fun.protect ~finally fn;

      let ic = open_in fname in
      let finally _ = close_in ic in
      let fn _ =
        ic
          |> Lexer.from_channel
          |> assert_lexes ~ctxt ""
      in
      Fun.protect ~finally fn;
    in
    let test_from_file ctxt =
      let (fname, oc) = Filename.open_temp_file "" "" in
      output_string oc code;
      close_out oc;

      fname
        |> Lexer.from_file
        |> assert_lexes ~ctxt fname
    in
    "Constructors" >::: [
      "From String"        >:: test_from_string;
      "From Input Channel" >:: test_from_channel;
      "From File"          >:: test_from_file;
    ]
  in
  let test_equal =
    let loc = Loc.mock "fname" (1, 2, 3) (4, 5, 6) in

    let eof = Parser.EOF loc in
    let punc_lparen = Parser.LPAREN loc in
    let punc_rparen = Parser.RPAREN loc in
    let punc_colon = Parser.COLON loc in
    let punc_arrow = Parser.ARROW loc in
    let punc_bind = Parser.BIND loc in
    let punc_comma = Parser.COMMA loc in
    let kwd_let = Parser.LET loc in
    let kwd_rec = Parser.REC loc in
    let kwd_and = Parser.AND loc in
    let kwd_in = Parser.IN loc in
    let kwd_if = Parser.IF loc in
    let kwd_then = Parser.THEN loc in
    let kwd_else = Parser.ELSE loc in
    let op_add = Parser.ADD loc in
    let op_sub = Parser.SUB loc in
    let op_mul = Parser.MUL loc in
    let op_div = Parser.DIV loc in
    let op_mod = Parser.MOD loc in
    let op_and = Parser.LAND loc in
    let op_or = Parser.LOR loc in
    let op_not = Parser.LNOT loc in
    let op_eq = Parser.EQ loc in
    let op_neq = Parser.NEQ loc in
    let op_lte = Parser.LTE loc in
    let op_lt = Parser.LT loc in
    let op_gt = Parser.GT loc in
    let op_gte = Parser.GTE loc in
    let lit_true = Parser.BOOL (loc, true) in
    let lit_false = Parser.BOOL (loc, false) in
    let lit_int_0 = Parser.INT (loc, 0) in
    let lit_int_1 = Parser.INT (loc, 1) in
    let uident_one = Parser.UIDENT (loc, "One") in
    let uident_two = Parser.UIDENT (loc, "Two") in
    let lident_one = Parser.UIDENT (loc, "one") in
    let lident_two = Parser.UIDENT (loc, "two") in

    let non_printable = [eof] in
    let punc = [
      punc_lparen; punc_rparen;
      punc_colon; punc_arrow;
      punc_bind; punc_comma;
    ] in
    let kwd = [
      kwd_let; kwd_rec; kwd_and; kwd_in;
      kwd_if; kwd_then; kwd_else;
    ] in
    let op = [
      op_add; op_sub; op_mul; op_div; op_mod;
      op_and; op_or; op_not;
      op_eq; op_neq;
      op_lte; op_lt; op_gt; op_gte;
    ] in
    let lit = [
      lit_true; lit_false;
      lit_int_1; lit_int_0;
    ] in
    let ident = [
      uident_one; uident_two;
      lident_one; lident_two;
    ] in

    let assert_tok_equality ~ctxt ne tok =
      let not_equal x y = not (Lexer.equal x y) in
      tok
        |> assert_equal ~ctxt ~cmp:Lexer.equal tok;
      tok
        |> Lexer.deloc
        |> assert_equal ~ctxt ~cmp:not_equal tok;
      let iter tok' =
        tok
          |> assert_equal ~ctxt ~cmp:not_equal tok';
        tok
          |> Lexer.deloc
          |> assert_equal ~ctxt ~cmp:not_equal tok';
      in
      List.iter iter ne
    in

    let test_non_printable =
      let test_eof ctxt =
        eof
          |> assert_tok_equality ~ctxt (punc @ kwd @ op @ lit @ ident)
      in
      "Non-Printable" >::: [
        "End-Of-File" >:: test_eof;
      ]
    in
    let test_punctuation =
      let assert_punc ~ctxt neq_punc tok =
        tok
          |> assert_tok_equality ~ctxt (non_printable @ neq_punc @ kwd @ op @ lit @ ident)
      in

      let test_punc_lparen ctxt =
        punc_lparen
          |> assert_punc ~ctxt [punc_rparen; punc_colon; punc_arrow; punc_bind; punc_comma]
      in
      let test_punc_rparen ctxt =
        punc_rparen
          |> assert_punc ~ctxt [punc_lparen; punc_colon; punc_arrow; punc_bind; punc_comma]
      in
      let test_punc_colon ctxt =
        punc_colon
          |> assert_punc ~ctxt [punc_lparen; punc_rparen; punc_arrow; punc_bind; punc_comma]
      in
      let test_punc_arrow ctxt =
        punc_arrow
          |> assert_punc ~ctxt [punc_lparen; punc_rparen; punc_colon; punc_bind; punc_comma]
      in
      let test_punc_bind ctxt =
        punc_bind
          |> assert_punc ~ctxt [punc_lparen; punc_rparen; punc_colon; punc_arrow; punc_comma]
      in
      let test_punc_comma ctxt =
        punc_comma
          |> assert_punc ~ctxt [punc_lparen; punc_rparen; punc_colon; punc_arrow; punc_bind]
      in
      "Punctuation" >::: [
        "Left Parenthesis"  >:: test_punc_lparen;
        "Right Parenthesis" >:: test_punc_rparen;
        "Colon"             >:: test_punc_colon;
        "Arrow"             >:: test_punc_arrow;
        "Bind"              >:: test_punc_bind;
        "Comma"             >:: test_punc_comma;
      ]
    in
    let test_keywords =
      let assert_kwd ~ctxt neq_kwd tok =
        tok
          |> assert_tok_equality ~ctxt (non_printable @ punc @ neq_kwd @ op @ lit @ ident)
      in

      let test_kwd_let ctxt =
        kwd_let
          |> assert_kwd ~ctxt [kwd_rec; kwd_and; kwd_in; kwd_if; kwd_then; kwd_else]
      in
      let test_kwd_rec ctxt =
        kwd_rec
          |> assert_kwd ~ctxt [kwd_let; kwd_and; kwd_in; kwd_if; kwd_then; kwd_else]
      in
      let test_kwd_and ctxt =
        kwd_and
          |> assert_kwd ~ctxt [kwd_let; kwd_rec; kwd_in; kwd_if; kwd_then; kwd_else]
      in
      let test_kwd_in ctxt =
        kwd_in
          |> assert_kwd ~ctxt [kwd_let; kwd_rec; kwd_and; kwd_if; kwd_then; kwd_else]
      in
      let test_kwd_if ctxt =
        kwd_if
          |> assert_kwd ~ctxt [kwd_let; kwd_rec; kwd_and; kwd_in; kwd_then; kwd_else]
      in
      let test_kwd_then ctxt =
        kwd_then
          |> assert_kwd ~ctxt [kwd_let; kwd_rec; kwd_and; kwd_in; kwd_if; kwd_else]
      in
      let test_kwd_else ctxt =
        kwd_else
          |> assert_kwd ~ctxt [kwd_let; kwd_rec; kwd_and; kwd_in; kwd_if; kwd_then]
      in
      "Keywords" >::: [
        "Let"  >:: test_kwd_let;
        "Rec"  >:: test_kwd_rec;
        "And"  >:: test_kwd_and;
        "In"   >:: test_kwd_in;
        "If"   >:: test_kwd_if;
        "Then" >:: test_kwd_then;
        "Else" >:: test_kwd_else;
      ]
    in
    let test_operators =
      let assert_op ~ctxt neq_op tok =
        tok
          |> assert_tok_equality ~ctxt (non_printable @ punc @ kwd @ neq_op @ lit @ ident)
      in

      let test_op_add ctxt =
        op_add
          |> assert_op ~ctxt [op_sub; op_mul; op_div; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_sub ctxt =
        op_sub
          |> assert_op ~ctxt [op_add; op_mul; op_div; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_mul ctxt =
        op_mul
          |> assert_op ~ctxt [op_add; op_sub; op_div; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_div ctxt =
        op_div
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_mod ctxt =
        op_mod
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_and ctxt =
        op_and
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_or ctxt =
        op_or
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_not; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_not ctxt =
        op_not
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_or; op_eq; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_eq ctxt =
        op_eq
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_or; op_not; op_neq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_neq ctxt =
        op_neq
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_or; op_not; op_eq; op_lte; op_lt; op_gt; op_gte]
      in
      let test_op_lte ctxt =
        op_lte
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lt; op_gt; op_gte]
      in
      let test_op_lt ctxt =
        op_lt
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_gt; op_gte]
      in
      let test_op_gt ctxt =
        op_gt
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gte]
      in
      let test_op_gte ctxt =
        op_gte
          |> assert_op ~ctxt [op_add; op_sub; op_mul; op_div; op_mod; op_and; op_or; op_not; op_eq; op_neq; op_lte; op_lt; op_gt]
      in
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
        "Less Then"             >:: test_op_lt;
        "Greater Than"          >:: test_op_gt;
        "Greater Than or Equal" >:: test_op_gte;
      ]
    in
    let test_literals =
      let assert_lit ~ctxt neq_lit tok =
        tok
          |> assert_tok_equality ~ctxt (non_printable @ punc @ kwd @ op @ neq_lit @ ident)
      in

      let test_lit_true ctxt =
        lit_true
          |> assert_lit ~ctxt [lit_false; lit_int_0; lit_int_1]
      in
      let test_lit_false ctxt =
        lit_false
          |> assert_lit ~ctxt [lit_true; lit_int_0; lit_int_1]
      in
      let test_lit_int ctxt =
        lit_int_0
          |> assert_lit ~ctxt [lit_true; lit_false; lit_int_1];
        lit_int_1
          |> assert_lit ~ctxt [lit_true; lit_false; lit_int_0]
      in
      "Literals" >::: [
        "True"    >:: test_lit_true;
        "False"   >:: test_lit_false;
        "Integer" >:: test_lit_int;
      ]
    in
    let test_identifiers =
      let assert_ident ~ctxt neq_ident tok =
        tok
          |> assert_tok_equality ~ctxt (non_printable @ punc @ kwd @ op @ lit @ neq_ident)
      in

      let test_uident ctxt =
        uident_one
          |> assert_ident ~ctxt [uident_two; lident_one; lident_two];
        uident_two
          |> assert_ident ~ctxt [uident_one; lident_one; lident_two]
      in
      let test_lident ctxt =
        lident_one
          |> assert_ident ~ctxt [uident_one; uident_two; lident_two];
        lident_two
          |> assert_ident ~ctxt [uident_one; uident_two; lident_one]
      in
      "Identifiers" >::: [
        "Upper Case" >:: test_uident;
        "Lower Case" >:: test_lident;
      ]
    in
    "Token Equality" >::: [
      test_non_printable;
      test_punctuation;
      test_keywords;
      test_operators;
      test_literals;
      test_identifiers;
    ]
  in
  let test_tokens =
    let assert_lexes ~ctxt expected str =
      let tok =
        Lexer.from_string str
          |> Lexer.lex
      in

      let len = String.length str in
      let expected_loc = Loc.mock "-" (1, 0, 0) (1, len, len) in
      tok
        |> Lexer.loc
        |> assert_equal ~ctxt ~cmp:Loc.equal expected_loc;
      tok
        |> Lexer.deloc
        |> assert_equal ~ctxt ~cmp:Lexer.equal expected
    in

    let test_constructors =
      let test_non_printable =
        let test_eof ctxt =
          ""
            |> assert_lexes ~ctxt (Parser.EOF Loc.dummy)
        in
        let test_newline ctxt =
          let str = "   \n  foo\n  " in

          let tok =
            Lexer.from_string str
              |> Lexer.lex
          in

          let expected_loc = Loc.mock "-" (2, 2, 6) (2, 5, 9) in
          tok
            |> Lexer.loc
            |> assert_equal ~ctxt ~cmp:Loc.equal expected_loc;
          tok
            |> Lexer.deloc
            |> assert_equal ~ctxt ~cmp:Lexer.equal (Parser.LIDENT (Loc.dummy, "foo"))
        in
        "Non-Printable" >::: [
          "End-Of-File" >:: test_eof;
          "New Line"    >:: test_newline;
        ]
      in
      let test_punctuation =
        let test_punc_lparen ctxt =
          "("
            |> assert_lexes ~ctxt (Parser.LPAREN Loc.dummy)
        in
        let test_punc_rparen ctxt =
          ")"
            |> assert_lexes ~ctxt (Parser.RPAREN Loc.dummy)
        in
        let test_punc_colon ctxt =
          ":"
            |> assert_lexes ~ctxt (Parser.COLON Loc.dummy)
        in
        let test_punc_arrow ctxt =
          "->"
            |> assert_lexes ~ctxt (Parser.ARROW Loc.dummy)
        in
        let test_punc_bind ctxt =
          "="
            |> assert_lexes ~ctxt (Parser.BIND Loc.dummy)
        in
        let test_punc_comma ctxt =
          ","
            |> assert_lexes ~ctxt (Parser.COMMA Loc.dummy)
        in
        "Punctuation" >::: [
          "Left Parenthesis"  >:: test_punc_lparen;
          "Right Parenthesis" >:: test_punc_rparen;
          "Colon"             >:: test_punc_colon;
          "Arrow"             >:: test_punc_arrow;
          "Bind"              >:: test_punc_bind;
          "Comma"             >:: test_punc_comma;
        ]
      in
      let test_keywords =
        let test_kwd_let ctxt =
          "let"
            |> assert_lexes ~ctxt (Parser.LET Loc.dummy)
        in
        let test_kwd_rec ctxt =
          "rec"
            |> assert_lexes ~ctxt (Parser.REC Loc.dummy)
        in
        let test_kwd_and ctxt =
          "and"
            |> assert_lexes ~ctxt (Parser.AND Loc.dummy)
        in
        let test_kwd_in ctxt =
          "in"
            |> assert_lexes ~ctxt (Parser.IN Loc.dummy)
        in
        let test_kwd_if ctxt =
          "if"
            |> assert_lexes ~ctxt (Parser.IF Loc.dummy)
        in
        let test_kwd_then ctxt =
          "then"
            |> assert_lexes ~ctxt (Parser.THEN Loc.dummy)
        in
        let test_kwd_else ctxt =
          "else"
            |> assert_lexes ~ctxt (Parser.ELSE Loc.dummy)
        in
        "Keywords" >::: [
          "Let"  >:: test_kwd_let;
          "Rec"  >:: test_kwd_rec;
          "And"  >:: test_kwd_and;
          "In"   >:: test_kwd_in;
          "If"   >:: test_kwd_if;
          "Then" >:: test_kwd_then;
          "Else" >:: test_kwd_else;
        ]
      in
      let test_operators =
        let test_op_add ctxt =
          "+"
            |> assert_lexes ~ctxt (Parser.ADD Loc.dummy)
        in
        let test_op_sub ctxt =
          "-"
            |> assert_lexes ~ctxt (Parser.SUB Loc.dummy)
        in
        let test_op_mul ctxt =
          "*"
            |> assert_lexes ~ctxt (Parser.MUL Loc.dummy)
        in
        let test_op_div ctxt =
          "/"
            |> assert_lexes ~ctxt (Parser.DIV Loc.dummy)
        in
        let test_op_mod ctxt =
          "%"
            |> assert_lexes ~ctxt (Parser.MOD Loc.dummy)
        in
        let test_op_and ctxt =
          "&&"
            |> assert_lexes ~ctxt (Parser.LAND Loc.dummy)
        in
        let test_op_or ctxt =
          "||"
            |> assert_lexes ~ctxt (Parser.LOR Loc.dummy)
        in
        let test_op_not ctxt =
          "!"
            |> assert_lexes ~ctxt (Parser.LNOT Loc.dummy)
        in
        let test_op_eq ctxt =
          "=="
            |> assert_lexes ~ctxt (Parser.EQ Loc.dummy)
        in
        let test_op_neq ctxt =
          "!="
            |> assert_lexes ~ctxt (Parser.NEQ Loc.dummy)
        in
        let test_op_lte ctxt =
          "<="
            |> assert_lexes ~ctxt (Parser.LTE Loc.dummy)
        in
        let test_op_lt ctxt =
          "<"
            |> assert_lexes ~ctxt (Parser.LT Loc.dummy)
        in
        let test_op_gt ctxt =
          ">"
            |> assert_lexes ~ctxt (Parser.GT Loc.dummy)
        in
        let test_op_gte ctxt =
          ">="
            |> assert_lexes ~ctxt (Parser.GTE Loc.dummy)
        in
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
        ]
      in
      let test_literals =
        let test_lit_true ctxt =
          "true"
            |> assert_lexes ~ctxt (Parser.BOOL (Loc.dummy, true))
        in
        let test_lit_false ctxt =
          "false"
            |> assert_lexes ~ctxt (Parser.BOOL (Loc.dummy, false))
        in
        let test_lit_int ctxt =
          "0"
            |> assert_lexes ~ctxt (Parser.INT (Loc.dummy, 0));
          "42"
            |> assert_lexes ~ctxt (Parser.INT (Loc.dummy, 42));
          "+42"
            |> assert_lexes ~ctxt (Parser.INT (Loc.dummy, 42));
          "-42"
            |> assert_lexes ~ctxt (Parser.INT (Loc.dummy, (-42)))
        in
        "Literals" >::: [
          "True"    >:: test_lit_true;
          "False"   >:: test_lit_false;
          "Integer" >:: test_lit_int;
        ]
      in
      let test_identifiers =
        let test_uident ctxt =
          "Id"
            |> assert_lexes ~ctxt (Parser.UIDENT (Loc.dummy, "Id"))
        in
        let test_lident ctxt =
          "id"
            |> assert_lexes ~ctxt (Parser.LIDENT (Loc.dummy, "id"))
        in
        "Identifiers" >::: [
          "Upper Case" >:: test_uident;
          "Lower Case" >:: test_lident;
        ]
      in
      "Constructors" >::: [
        test_non_printable;
        test_punctuation;
        test_keywords;
        test_operators;
        test_literals;
        test_identifiers;
      ]
    in
    let test_deloc =
      let loc = Loc.mock "fname" (1, 2, 3) (4, 5, 6) in

      let assert_deloc ~ctxt expected actual =
        actual
          |> Lexer.deloc
          |> assert_equal ~ctxt ~cmp:Lexer.equal expected
      in

      let test_non_printable =
        let test_eof ctxt =
          Parser.EOF loc
            |> assert_deloc ~ctxt (Parser.EOF Loc.dummy)
        in
        "Non-Printable" >::: [
          "End-Of-File" >:: test_eof;
        ]
      in
      let test_punctuation =
        let test_punc_lparen ctxt =
          Parser.LPAREN loc
            |> assert_deloc ~ctxt (Parser.LPAREN Loc.dummy)
        in
        let test_punc_rparen ctxt =
          Parser.RPAREN loc
            |> assert_deloc ~ctxt (Parser.RPAREN Loc.dummy)
        in
        let test_punc_colon ctxt =
          Parser.COLON loc
            |> assert_deloc ~ctxt (Parser.COLON Loc.dummy)
        in
        let test_punc_arrow ctxt =
          Parser.ARROW loc
            |> assert_deloc ~ctxt (Parser.ARROW Loc.dummy)
        in
        let test_punc_bind ctxt =
          Parser.BIND loc
            |> assert_deloc ~ctxt (Parser.BIND Loc.dummy)
        in
        let test_punc_comma ctxt =
          Parser.COMMA loc
            |> assert_deloc ~ctxt (Parser.COMMA Loc.dummy)
        in
        "Punctuation" >::: [
          "Left Parenthesis"  >:: test_punc_lparen;
          "Right Parenthesis" >:: test_punc_rparen;
          "Colon"             >:: test_punc_colon;
          "Arrow"             >:: test_punc_arrow;
          "Bind"              >:: test_punc_bind;
          "Comma"             >:: test_punc_comma;
        ]
      in
      let test_keywords =
        let test_kwd_let ctxt =
          Parser.LET loc
            |> assert_deloc ~ctxt (Parser.LET Loc.dummy)
        in
        let test_kwd_rec ctxt =
          Parser.REC loc
            |> assert_deloc ~ctxt (Parser.REC Loc.dummy)
        in
        let test_kwd_and ctxt =
          Parser.AND loc
            |> assert_deloc ~ctxt (Parser.AND Loc.dummy)
        in
        let test_kwd_in ctxt =
          Parser.IN loc
            |> assert_deloc ~ctxt (Parser.IN Loc.dummy)
        in
        let test_kwd_if ctxt =
          Parser.IF loc
            |> assert_deloc ~ctxt (Parser.IF Loc.dummy)
        in
        let test_kwd_then ctxt =
          Parser.THEN loc
            |> assert_deloc ~ctxt (Parser.THEN Loc.dummy)
        in
        let test_kwd_else ctxt =
          Parser.ELSE loc
            |> assert_deloc ~ctxt (Parser.ELSE Loc.dummy)
        in
        "Keywords" >::: [
          "Let"  >:: test_kwd_let;
          "Rec"  >:: test_kwd_rec;
          "And"  >:: test_kwd_and;
          "In"   >:: test_kwd_in;
          "If"   >:: test_kwd_if;
          "Then" >:: test_kwd_then;
          "Else" >:: test_kwd_else;
        ]
      in
      let test_operators =
        let test_op_add ctxt =
          Parser.ADD loc
            |> assert_deloc ~ctxt (Parser.ADD Loc.dummy)
        in
        let test_op_sub ctxt =
          Parser.SUB loc
            |> assert_deloc ~ctxt (Parser.SUB Loc.dummy)
        in
        let test_op_mul ctxt =
          Parser.MUL loc
            |> assert_deloc ~ctxt (Parser.MUL Loc.dummy)
        in
        let test_op_div ctxt =
          Parser.DIV loc
            |> assert_deloc ~ctxt (Parser.DIV Loc.dummy)
        in
        let test_op_mod ctxt =
          Parser.MOD loc
            |> assert_deloc ~ctxt (Parser.MOD Loc.dummy)
        in
        let test_op_and ctxt =
          Parser.LAND loc
            |> assert_deloc ~ctxt (Parser.LAND Loc.dummy)
        in
        let test_op_or ctxt =
          Parser.LOR loc
            |> assert_deloc ~ctxt (Parser.LOR Loc.dummy)
        in
        let test_op_not ctxt =
          Parser.LNOT loc
            |> assert_deloc ~ctxt (Parser.LNOT Loc.dummy)
        in
        let test_op_eq ctxt =
          Parser.EQ loc
            |> assert_deloc ~ctxt (Parser.EQ Loc.dummy)
        in
        let test_op_neq ctxt =
          Parser.NEQ loc
            |> assert_deloc ~ctxt (Parser.NEQ Loc.dummy)
        in
        let test_op_lte ctxt =
          Parser.LTE loc
            |> assert_deloc ~ctxt (Parser.LTE Loc.dummy)
        in
        let test_op_lt ctxt =
          Parser.LT loc
            |> assert_deloc ~ctxt (Parser.LT Loc.dummy)
        in
        let test_op_gt ctxt =
          Parser.GT loc
            |> assert_deloc ~ctxt (Parser.GT Loc.dummy)
        in
        let test_op_gte ctxt =
          Parser.GTE loc
            |> assert_deloc ~ctxt (Parser.GTE Loc.dummy)
        in
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
        ]
      in
      let test_literals =
        let test_lit_bool ctxt =
          Parser.BOOL (loc, true)
            |> assert_deloc ~ctxt (Parser.BOOL (Loc.dummy, true));
          Parser.BOOL (loc, false)
            |> assert_deloc ~ctxt (Parser.BOOL (Loc.dummy, false))
        in
        let test_lit_int ctxt =
          Parser.INT (loc, 42)
            |> assert_deloc ~ctxt (Parser.INT (Loc.dummy, 42))
        in
        "Literals" >::: [
          "Boolean" >:: test_lit_bool;
          "Integer" >:: test_lit_int;
        ]
      in
      let test_identifiers =
        let test_uident ctxt =
          Parser.UIDENT (loc, "Id")
            |> assert_deloc ~ctxt (Parser.UIDENT (Loc.dummy, "Id"))
        in
        let test_lident ctxt =
          Parser.LIDENT (loc, "Id")
            |> assert_deloc ~ctxt (Parser.LIDENT (Loc.dummy, "Id"))
        in
        "Identifiers" >::: [
          "Upper Case" >:: test_uident;
          "Lower Case" >:: test_lident;
        ]
      in
      "Strip Location Information" >::: [
        test_non_printable;
        test_punctuation;
        test_keywords;
        test_operators;
        test_literals;
        test_identifiers;
      ]
    in
    "Tokens" >::: [
      test_constructors;
      test_deloc;
    ]
  in
  "Lexer" >::: [
    test_constructors;
    test_equal;
    test_tokens;
  ]
