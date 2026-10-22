# Programming Methodologies (OCaml)

[![OCaml](https://img.shields.io/badge/OCaml-5.x-orange.svg)](https://ocaml.org)
[![Build System](https://img.shields.io/badge/build-dune-blue.svg)](https://dune.build/)
[![Course](https://img.shields.io/badge/course-Metody%20Programowania%20(UWr)-informational.svg)](https://cs.uni.wroc.pl)

A curated collection of language interpreters, compilers, virtual machines, type inference engines, and coursework projects developed for the **Programming Methodologies** (*Metody Programowania*) course at the **University of Wrocław** (*Uniwersytet Wrocławski*).

---

## Table of Contents

- [Overview](#overview)
- [Repository Structure](#repository-structure)
  - [1. Interpreters](#1-interpreters)
  - [2. Compilers & Virtual Machines](#2-compilers--virtual-machines)
  - [3. Type Systems](#3-type-systems)
  - [4. Projects & Coursework](#4-projects--coursework)
  - [5. Problem Sets & Exercises](#5-problem-sets--exercises)
- [Prerequisites & Build Instructions](#prerequisites--build-instructions)
- [License](#license)

---

## Overview

This repository demonstrates the step-by-step construction of programming language implementations in **OCaml**:
- Scanning and parsing using **Menhir** and **ocamllex**
- Abstract Syntax Trees (AST), environments, and operational semantics
- Control flow, imperative state, and AST optimization passes
- Virtual machine architectures (stack, accumulator, and Motorola 6809 register machine)
- Type checking, type inference (Algorithm W with unification), and polymorphic type systems
- Relational query evaluation and SAT/QBF solving

---

## Repository Structure

```
metody/
├── interpreters/          # Core language interpreters
│   ├── calc/             # Basic arithmetic expression evaluator
│   ├── let/              # Scoped variable binding language (let x = e1 in e2)
│   ├── fun/              # Functional language with closures & recursion
│   ├── while/            # Imperative language with while-loops & AST optimizer
│   └── while-lecture/    # Lecture reference implementation of While
│
├── compilers-and-vm/     # Code generators and target architectures
│   ├── calc-acc/         # Accumulator-based virtual machine compiler
│   ├── calc-6809/        # Motorola 6809 bytecode compiler & assembler
│   ├── emu6809/          # Motorola 6809 microprocessor emulator (C)
│   └── extensions/       # Compiler feature additions
│       ├── calc-if/      # Conditionals support (if-then-else)
│       ├── calc-fun/     # Function definitions & calls
│       └── calc-opt/     # Constant folding & optimization passes
│
├── type-systems/         # Static semantics & type theory
│   ├── tcalc-simple/     # Simply-typed calculator & type checker
│   ├── tcalc-infer/      # Type inference via unification (Algorithm W)
│   └── tcalc-poly/       # Parametric polymorphism (Hindley-Milner system)
│
├── projects/             # Comprehensive course projects
│   ├── table-query-evaluator/ # Relational table query engine (filter, limit, add_column)
│   ├── prac3-interpreter/     # Language with first-class functions, tuples & non-local exits
│   ├── qbf-solver/            # Quantified Boolean Formula (QBF) evaluator
│   └── unification/           # Imperative term unification algorithm
│
└── problem-sets/         # Solutions to theoretical & practical problem sheets
    ├── list02_fibonacci_matrix.txt     # Matrix exponentiation & recursion
    ├── list03_trees_and_folds.txt      # Functional combinators & tree folds
    ├── list04_structural_induction.txt # Structural induction proofs (lists, trees)
    ├── list05_grammars.txt             # Context-free grammars & ambiguity
    ├── list06_parser_extensions.txt    # Parser grammar extensions
    ├── list09_stack_machine.txt        # Stack machine execution traces
    └── fib_rec_project/                # Dune benchmark project for Fibonacci
```

---

### 1. Interpreters

* [interpreters/calc](interpreters/calc): Arithmetic calculator supporting addition, subtraction, multiplication, and division.
* [interpreters/let](interpreters/let): Extends expressions with lexical scopes and variable bindings (`let x = e1 in e2`).
* [interpreters/fun](interpreters/fun): Higher-order functional language supporting anonymous functions (`fun x -> ...`), multi-argument application, closures, and recursive definitions (`let rec`).
* [interpreters/while](interpreters/while): An imperative language interpreter with variables, sequential execution, while loops, conditional statements, AST transformations, and an AST pretty-printer.
* [interpreters/while-lecture](interpreters/while-lecture): Lecture implementation including Polish notation translation and classic algorithmic benchmarks (Fibonacci, Euclid's GCD, Pascal's triangle, sieve).

### 2. Compilers & Virtual Machines

* [compilers-and-vm/calc-acc](compilers-and-vm/calc-acc): Compiles arithmetic expressions to an accumulator-based virtual machine instruction set.
* [compilers-and-vm/calc-6809](compilers-and-vm/calc-6809): End-to-end compiler translating expressions into Motorola 6809 machine instructions.
* [compilers-and-vm/emu6809](compilers-and-vm/emu6809): Cycle-accurate emulator for the Motorola 6809 CPU written in C, including memory bus, CPU registers, and instruction decoding.
* [compilers-and-vm/extensions](compilers-and-vm/extensions):
  * **calc-if**: Adds boolean types, comparisons, and branch instructions.
  * **calc-fun**: Adds function prologue/epilogue generation, call stack management, and argument passing.
  * **calc-opt**: Implements constant propagation, arithmetic simplification, and dead code elimination.

### 3. Type Systems

* [type-systems/tcalc-simple](type-systems/tcalc-simple): Static type checker for integers, booleans, pairs, and function types ($T_1 \to T_2$).
* [type-systems/tcalc-infer](type-systems/tcalc-infer): Hindley-Milner type reconstruction using unification variables and constraint solving.
* [type-systems/tcalc-poly](type-systems/tcalc-poly): Polymorphic type system featuring universal quantification ($\forall \alpha$), type schemes, and generalization/instantiation.

### 4. Projects & Coursework

* [projects/table-query-evaluator](projects/table-query-evaluator): Relational data processor implementing table queries (`Limit`, `DropColumn`, `RowNumber`, `Filter`, `AddColumn`) over typed table environments.
* [projects/prac3-interpreter](projects/prac3-interpreter): Extensive interpreter featuring first-class functions, n-ary tuples with indexing, and lexical return/exception handling.
* [projects/qbf-solver](projects/qbf-solver): Solver for Quantified Boolean Formulas evaluating $\forall$ and $\exists$ quantifiers over boolean propositions.
* [projects/unification](projects/unification): Fast graph-based imperative term unification using mutable references and path compression.

### 5. Problem Sets & Exercises

* [problem-sets/](problem-sets): Weekly exercise sheets detailing:
  * Structural induction proofs over algebraic data types.
  * Context-free grammar disambiguation and associativity rules.
  * Time and space complexity analysis of functional algorithms.
  * Stack machine trace execution and compiler correctness.

---

## Prerequisites & Build Instructions

### Requirements

- **OCaml** $\ge$ 4.14 (or 5.x)
- **OPAM** package manager
- **Dune** $\ge$ 2.0 / 3.x
- **Menhir** parser generator

### Setup

```bash
# Initialize OPAM switch (if needed)
opam switch create 5.3.0
eval $(opam env)

# Install build tools
opam install dune menhir
```

### Building Projects

Each subproject contains its own `dune-project` and can be built independently:

```bash
# Build any interpreter, for example:
cd interpreters/fun
dune build

# Run executable or tests:
dune exec bin/main.exe
```

To build the Motorola 6809 emulator:
```bash
cd compilers-and-vm/emu6809
make
```

---

## License

This repository is maintained for educational purposes. Code developed as part of the University of Wrocław Computer Science curriculum.
