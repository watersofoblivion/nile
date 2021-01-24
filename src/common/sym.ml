(* Symbolization *)

(* Symbols *)

type sym = int

(* Name-to-Symbol Tables *)

module NameMap = Map.Make (struct
  type t = string
  let compare = compare
end)

type tbl = {
  idx:  sym;
  syms: sym NameMap.t;
}

let tbl =
  { idx  = 0;
    syms = NameMap.empty }

let symbolize str tbl =
  try (NameMap.find str tbl.syms, tbl)
  with Not_found ->
    let tbl' =
      { idx  = tbl.idx + 1;
        syms = NameMap.add str tbl.idx tbl.syms }
    in
    (tbl.idx, tbl')

(* Symbol-to-Name Mapping *)

module SymMap = Map.Make (struct
  type t = sym
  let compare = compare
end)

type names = string SymMap.t

let names tbl =
  let fold name sym mapping = SymMap.add sym name mapping in
  NameMap.fold fold SymMap.empty tbl.syms

let name_of = SymMap.find
