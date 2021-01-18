(* Built-in Function Types *)

let builtins = [
  ("printBool", Type.func Type.bool Type.unit);
  ("printInt",  Type.func Type.int  Type.unit);
  ("printLn",   Type.func Type.unit Type.unit)
]
