---
title: Goals
linkTitle: Goals
description: |
  High-level overview of the project's goals and non-goals
weight: 10
---

The focus of this project is mostly educational: to create a quality compiler for a known language from scratch.  The focus is purely on the compiler itself, not a creating a full language distribution.  It is also not focused on implementing novel language features: the language it will compile is a nearly-pure subset of Core ML.

Goals
---

* **Core ML** - The target language is a nearly pure subset of the Core ML language, akin to SML.  Advanced typing or language extensions present in dialects such as OCaml are not targeted.
* **Optimization** - The compiler should generate efficient executable code, and should have a comprehensive optimizer.
* **Production Quality** - The compiler itself should be efficient and bug-free.
* **Tested** - The compiler should be unit and integration tested, with high code coverage.
* **Documented** - The syntax and semantics of the language the compiler accepts should be documented in a Language Specification, the command-line interface of the compiler should be documented in a User Guide, and the implementation should be documented using the build-in language tools.
* **Benchmarked** - Features should be benchmarked to objectively measure their effect on compiler and executable performance.  (Their impact on developer productivity is subjective.)

Non-Goals
---

* **Standard Library** - The language is not meant to be used for anything beyond a target for compilation.  While a stdlib would have a large amount of code to compile and exercise the language, it is a separate project with separate requirements orthogonal to the main compiler.
* **Mutability** - References and/or arrays will not be supported.  The only impure functionality that will be exposed is exceptions and enough primitive output functions to support integration testing / benchmarking.
* **Garbage Collection** - The compiler will not be used for long-running programs, so it doesn't need the complexity of a garbage collector.
* **Self-hosting** - The compiler will not be able to compile an implementation of itself, but only because of a lack of standard library and I/O functions, not because of any shortcoming of the language.
* **Multi-File Compilation** - The added complexity is only useful for compiling large projects, and the focus is purely on the compiler itself.
