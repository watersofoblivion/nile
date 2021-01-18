---
title: Papers and Articles
linkTitle: Papers and Articles
weight: 500
description: |
  Academic papers and articles on particular implementation details
hide_feedback: true
---

General
---

* [Orbit](https://cpsc.yale.edu/sites/default/files/files/tr445.pdf) (Kranz, et al., 1986) - Documents the implementation of the Orbit, an optimizing native-code compiler for the Scheme language.  Provides a good overview of how all of the pieces fit together to form a complete language.

Compiler Performance
---

* [Type-Safe Modular Hash-Consing](https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf) - Presents the technique of "hash-consing", which induces structural-sharing on internal representations of languages.  Also provides a specific implementation in OCaml.

Pretty-Printing
---

* [Format](https://ocaml.org/releases/4.10/htmlman/libref/Format.html) - The OCaml Format module, used for pretty-printing all internal representations.
* [Format Unraveled](https://hal.archives-ouvertes.fr/hal-01503081/file/format-unraveled.pdf) - An extended explanation of OCaml's Format module, with extensive examples.  Also includes a thorough discussion of OCaml's semantic tags.
* [Oppen's Algorithm](https://www.cs.tufts.edu/~nr/cs257/archive/derek-oppen/prettyprinting.pdf) - The original paper on the algorithm underlying OCaml's Format module.  Provides a complete description and implementation of the algorithm (in Pascal!).

Pattern Matching
---

* [A Term Pattern-Match Compiler Inspired by Finite Automata Theory](https://link.springer.com/content/pdf/10.1007%2F3-540-55984-1_24.pdf)
* [When Do Match-Compilation Heuristics Matter?](https://www.cs.purdue.edu/homes/suresh/502-Fall2008/papers/patternMatch-ramsey.pdf)
* [Compiling Pattern Matching to Good Decision Trees](http://pauillac.inria.fr/~maranget/papers/ml05e-maranget.pdf)

Intermediate Representation (Administrative Normal Form)
---

* [The Essence of Compiling with Continuations](https://slang.soe.ucsc.edu/cormac/papers/pldi93.pdf) - Presents Administrative Normal Form (ANF) as a simplified, direct form of Continuation-Passing Style (CPS) intermediate representation.
* [Compiling Without Continuations](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/compiling-without-continuations.pdf) - Extends ANF with "join points" and "jumps", to reduce the number of closures created and produce more efficient code, especially by enabling cascading optimizations.  Also presents an algorithm for "contification", which converts closures to join points when possible.
* [Functional Intermediate Representations](https://xavierleroy.org/mpri/2-4/fir.pdf) - A paper comparing various intermediate representations for functional compilers.  Includes discussions of both CPS and ANF.

Optimization
---

### Inlining

* [Secrets of the Glasgow Haskell Compiler Inliner](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf) - A deep dive on the implementation of the Glasgow Haskell Compiler's inliner.  Provides several practical techniques to developing heuristics for when to inline functions.  Limited to first-order functions.
* [Pratical and Effective Higher-Order Optimization](http://manticore.cs.uchicago.edu/papers/icfp14-reflow.pdf) - Presents an algorithm for higher-order function inlining based on a 0CFA control-flow analysis.  The algorithm described is used in the Manticore parallel ML compiler.


### Constant Propagation

* [Constant Propagation with Conditional Branches](https://www.cs.utexas.edu/users/lin/cs380c/wegman.pdf) - Describes constant propagation as a dataflow problem and presents four different algorithms: Simple Constant, Sparse Simple Constant, Conditional Constant, and Sparse Conditional Constant Propagation.
* [A Functional Perspective on SSA Optimisation Algorithms](https://www.researchgate.net/publication/220371229_A_Functional_Perspective_on_SSA_Optimisation_Algorithms/fulltext/0ffc67ff0cf255165fc8561a/A-Functional-Perspective-on-SSA-Optimisation-Algorithms.pdf) - Presents and compares the implementation of the Condition Constant Propagation algorithm on both SSA form and in ANF form.


Code Generation
---

### Closure Conversion

* [Closure Conversion Is Safe for Space](https://www.cs.princeton.edu/~appel/papers/safe-closure.pdf) - Discusses space complexity for flat and linked closures, and shows that flat closures are safe for space complexity while linked closures are not.
* [Typed Closure Conversion](https://www.cs.cmu.edu/~rwh/papers/closures/popl96.pdf) - Discusses type safety for flat and linked closures.  Covers both monomorphic and polymorphic type systems.
* [Efficient and Safe-for-Space Closure Conversion](https://flint.cs.yale.edu/flint/publications/escc.pdf) - Presents an algorithm for Safe-for-Space Complexity closure conversion, which creates linked closures that are safe for space complexity.  The algorithm is based on Tarjan's strongly-connected components algorithm.

### Escape Analysis

* [Orbit](https://cpsc.yale.edu/sites/default/files/files/tr632.pdf) (Kranz, 1987) - A deep dive on the closure conversion and escape analysis portion of the Orbit compiler.  Presents techniques for generating efficient closures, allocating closures on the stack instead of the heap, and for eliminating certain closures all together.  Limited to first-order closures.
* [Higher Order Escape Analysis: Optimizing Stack Allocation in Functional Programming Implementations](https://link.springer.com/content/pdf/10.1007/3-540-52592-0_61.pdf) - Presents an algorithm for performing escape analysis on higher-order functions.

### Auto-boxing

* [Unboxed Objects and Polymorphic Typing](https://xavierleroy.org/publi/unboxed-polymorphism.pdf) - Presents a type-directed algorithm for unboxing polymorphic values to avoid unnecessary memory accesses.

### LLVM Code Generation

* [Kaleidoscope](https://llvm.org/docs/tutorial/index.html) - A tutorial on creating a simple compiler in OCaml using LLVM as a backend.  Provides a good (if slightly dated) introduction to the OCaml LLVM bindings.
* [Mapping High-Level Constructs to LLVM IR](https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/README.html) - A thorough and practical guide to generating LLVM IR.
* [LLVM Language Reference Manual](https://llvm.org/docs/LangRef.html) - The official documentation on LLVM IR.

Exceptions
---

* [Nico Brailovsky's Guide for Implementing the C++ Zero-Cost Exception ABI](https://monoinfinito.wordpress.com/category/programming/c/exceptions/) - A 21-part (plus appendices) guide to implementing the C++ Zero-Cost Exception ABI.  Provides a complete description and implementation in C of the ABI.
* [The New LLVM Exception Handling Scheme](https://llvm.org/devmtg/2011-09-16/EuroLLVM2011-ExceptionHandling.pdf) - A presentation on exception handling in LLVM using the C++ Zero-Cost Exception ABI
* [Exception Handling in LLVM, from Itanium to MSVC](https://llvm.org/devmtg/2015-10/slides/KlecknerMajnemer-ExceptionHandling.pdf) - A presentation discussing the differences in exception handling across multiple platforms, including on Windows.
* [LLVM Interface for C++ Zero-Cost Exception ABI](https://llvm.org/docs/ExceptionHandling.html) - Documents LLVM's support for the C++ Zero-Cost Exception ABI
