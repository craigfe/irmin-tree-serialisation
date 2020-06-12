(lang dune 2.0)
(name irmin-tree-serialisation)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github CraigFe/irmin-tree-serialisation))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name irmin-tree-serialisation)
 (synopsis "")
 (description "\

")
 (documentation https://CraigFe.github.io/irmin-tree-serialisation/)
 (depends
  (ocaml (>= 4.8.0))
  (irmin (= 2.1.0))
  (tezos-crypto (= 7.1))
  rusage
  fmt
  logs))
