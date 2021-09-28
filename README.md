## Description
Created mainly for a coding project, this is a simple implementation of a binary merkle tree in OCaml. binary.ml contains the binary tree implementation and merkle_trie.ml is an (unfinished) attempt at a merkle patricia tree implementation. 

## Running
Before running you must have both [Dune](https://github.com/ocaml/dune) and [Opam](https://opam.ocaml.org/) installed. 

First clone this repo and `cd merkle-tree`.

To install dependencies run
```
opam install . --deps-only
```

To run the tests defined in ./test.ml, run
```
 dune exec ./tests.exe
```
