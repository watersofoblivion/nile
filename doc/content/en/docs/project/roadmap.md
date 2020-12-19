---
title: Roadmap
linkTitle: Roadmap
description: |
  Planned future work and releases
weight: 20
hide_feedback: true
---

The plan is to release new user-facing frontend features in a series of v0.x.0 minor releases, and implement backend features to clean up and/or optimize those new features in a series of v0.x.y patch releases.

For each minor or patch releases, it is assumed that all released features:

* Are Unit tested with coverage approaching 100%
* Are Integration tested with coverage approaching 100%
* Have a Tutorial that explains their motivation and use cases
* Are documented at a tool level in the User Guide
* Are documented at a language level in the Language Spec
* Are documented at a code level in the Interface (.mli file)
* Have one or more benchmarks that exercises the feature

The planned features are listed below.  Later releases have intentionally been left vague and their requirements will be solidified once the previous minor/patch version has been released.  See the [Bibliography](/docs/bibliography) for references to implementation information.

v0.1.0
---

* Initial release
* Language Features
  * Booleans
  * Integers
  * First-Class Functions
  * Let bindings
  * Conditionals
  * Explicit Types
* ANF Intermediate Representation
* Flat Closure Conversion
* Conversion to LLVM assembly
* Linking with `clang` into an executable
* Type-checking for all internal and external representations
* Pretty-printers for all internal and external representations

v0.1.x
---

* Compiler Performance:
  * Hash-consing
  * Symbolization
* Target Language Performance:
  * Tail-call Optimization
  * Join Points and Contification
  * Function Inlining
  * Conditional Constant Propagation
  * Escape Analysis
  * Dead Code Elimination
  * Loop Invariant Hoisting
  * Improved Closure Conversion
    * Flat
    * Linked
    * Safe for Space Complexity
* Usability:
  * Parser error handling
  * Syntax highlighting
  * Multiple-error handling
  * Improved error output
  * Unused variable detection

v0.2.0
---

* Parametric Polymorphism (a.k.a, Generics)
* Tuples
* User-defined Types

v0.2.x
---

* Target Language Performance:
  * Auto-boxing
  * Code specialization

v0.3.0
---

* Recursive Types: Records and Variants
* Pattern Matching
* Strings
* Floats
* Operator overloading (Simple Subtyping)

v0.3.x
---

* Usability:
  * Exhaustiveness Check

v0.4.0
---

* Functional Data Structures:
  * Lists
  * Maps
  * Sets

v0.4.x
---

* Target Language Performance:
  * Specialized representations for primitive-typed data structures

v0.5.0
---

* Exceptions
