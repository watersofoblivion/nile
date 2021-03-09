(* Built-in Function Types *)

let builtins = [
  ("landBool", Type.func [Type.bool; Type.bool] Type.bool);
  ("lorBool", Type.func [Type.bool; Type.bool] Type.bool);
  ("lnotBool", Type.func [Type.bool] Type.bool);
  ("eqBool", Type.func [Type.bool; Type.bool] Type.bool);
  ("neqBool", Type.func [Type.bool; Type.bool] Type.bool);

  ("addInt", Type.func [Type.int; Type.int] Type.int);
  ("subInt", Type.func [Type.int; Type.int] Type.int);
  ("mulInt", Type.func [Type.int; Type.int] Type.int);
  ("divInt", Type.func [Type.int; Type.int] Type.int);
  ("modInt", Type.func [Type.int; Type.int] Type.int);
  ("eqInt", Type.func [Type.int; Type.int] Type.bool);
  ("neqInt", Type.func [Type.int; Type.int] Type.bool);
  ("lteInt", Type.func [Type.int; Type.int] Type.bool);
  ("ltInt", Type.func [Type.int; Type.int] Type.bool);
  ("gtInt", Type.func [Type.int; Type.int] Type.bool);
  ("gteInt", Type.func [Type.int; Type.int] Type.bool);

  ("addFloat", Type.func [Type.float; Type.float] Type.float);
  ("subFloat", Type.func [Type.float; Type.float] Type.float);
  ("mulFloat", Type.func [Type.float; Type.float] Type.float);
  ("divFloat", Type.func [Type.float; Type.float] Type.float);
  ("modFloat", Type.func [Type.float; Type.float] Type.float);
  ("eqFloat", Type.func [Type.float; Type.float] Type.bool);
  ("neqFloat", Type.func [Type.float; Type.float] Type.bool);
  ("lteFloat", Type.func [Type.float; Type.float] Type.bool);
  ("ltFloat", Type.func [Type.float; Type.float] Type.bool);
  ("gtFloat", Type.func [Type.float; Type.float] Type.bool);
  ("gteFloat", Type.func [Type.float; Type.float] Type.bool);

  ("concatString", Type.func [Type.string; Type.string] Type.string);

  ("concatBlob", Type.func [Type.blob; Type.blob] Type.blob)

  ("beforeTime", Type.func [Type.timestamp; Type.duration] Type.timestamp);
  ("afterTime", Type.func [Type.timestamp; Type.duration] Type.timestamp);
  ("eqTimestamp", Type.func [Type.timestamp; Type.timestamp] Type.bool);
  ("neqTimestamp", Type.func [Type.timestamp; Type.timestamp] Type.bool);
  ("lteTimestamp", Type.func [Type.timestamp; Type.timestamp] Type.bool);
  ("ltTimestamp", Type.func [Type.timestamp; Type.timestamp] Type.bool);
  ("gtTimestamp", Type.func [Type.timestamp; Type.timestamp] Type.bool);
  ("gteTimestamp", Type.func [Type.timestamp; Type.timestamp] Type.bool);

  ("addDuration", Type.func [Type.duration; Type.duration] Type.duration);
  ("subDuration", Type.func [Type.duration; Type.duration] Type.duration);
  ("eqDuration", Type.func [Type.duration; Type.duration] Type.duration);
  ("neqDuration", Type.func [Type.duration; Type.duration] Type.duration);
  ("lteDuration", Type.func [Type.duration; Type.duration] Type.duration);
  ("ltDuration", Type.func [Type.duration; Type.duration] Type.duration);
  ("gtDuration", Type.func [Type.duration; Type.duration] Type.duration);
  ("gteDuration", Type.func [Type.duration; Type.duration] Type.duration);
]
