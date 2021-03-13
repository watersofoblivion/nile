
type conf = {
  tailcall   : bool;
  inline     : bool;
  ccp        : bool;
  max_passes : int;
}

let conf tailcall inline ccp max_passes =
  { tailcall   = tailcall;
    inline     = inline;
    ccp        = ccp;
    max_passes = max_passes }
