---
title: Getting Started
linkTitle: Getting Started
weight: 100
description: |
  Get up and running with Nile in no time
hide_feedback: true
---

This will step you through getting the Nile compiler built and installed so you can write code in your new favorite language!

Building from Source
---

### Prerequisites

First, you need to install a few prerequsites:

* **Git** - The source is hosted on [GitHub](https://github.com/watersofoblivion/nile).  (This is pre-installed on OSX and most Linux distributions)
* **OCaml** - Nile is written in OCaml.
* **OPAM** - OPAM is a CPAN-like package manager for OCaml.  Nile is distributed as a OPAM package.
* **LLVM** - Nile generates LLVM bytecode instead of directly generating assembly.
* **Clang** - The Clang compiler is used to link the LLVM bytecode into a executable.  (This is the default C compiler pre-installed on OSX)

#### Mac OSX

Install the dependencies with [Homebrew](https://brew.sh) by running the following command:

```sh
brew install ocaml opam llvm
```

#### Linux

(Todo)

#### Windows

(Todo)

### Setup OPAM

OPAM requires an additional setup step to initialize the installation and download the list of available packages.  Perform this setup by running the two commands:

```sh
opam init
opam update
```

### Download the Source

The source for Nile is available on [GitHub](https://github.com/watersofoblivion/nile).  You can fetch it using `git clone`:

```sh
git clone https://github.com/watersofoblivion/nile
```

### Build the Compiler

The final step is to build the compiler with OPAM.  Assuming you cloned your source into a directory named `nile/`, it can be installed with the commands:

```sh
cd nile
opam install .
```

If all goes well, you should now have a `nile` executable in your path.

Writing Your First Nile Program
---
