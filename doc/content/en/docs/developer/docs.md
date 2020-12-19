---
title: Generated Documentation
linkTitle: Generated Documentation
weight: 950
description: |
  Documentation generated from the source code and other various tools.
hide_feedback: true
---

There are several sets of generated documentation available to developers.  Along with this site, each is rebuilt and deployed here as part of the continuous integration pipeline.

API Documentation
---

The implementation of Nile is documented at the code level using OCaml's [ODoc](https://github.com/ocaml/odoc) tool.  This includes detailed descriptions of the compiler's internals and is the authoritative source of information on the implementation (other than the source code itself.)  You can read the docs for the [main codebase](/static/odoc/nile/), the [unit tests and assertions](/static/odoc/test/unit/), the [integration tests and assertions](/static/odoc/test/integration/), or for the [benchmarking suite](/static/odoc/test/benchmark/)

Test Coverage
---

The implementation is instrumented with [Bisect](https://github.com/aantron/bisect_ppx) to produce test coverage information.  The latest coverage report is available for both the [unit tests](/static/coverage/unit/) and for the [integration tests](/static/coverage/integration/).

Benchmarks
---

Nile contains a suite of benchmarks that run the compiler with various sets configuration flags over a set of benchmark example programs designed to stress the compiler in various ways and collects statistics about the performance of both the compiler itself and the generated executable.  The data is aggregated across runs and a report is generated.  The latest version of the report is available [here](/static/benchmark/)
