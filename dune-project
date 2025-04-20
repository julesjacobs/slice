(lang dune 3.0)
(name contdice)
(generate_opam_files true)
(using menhir 2.1)

(package
 (name contdice)
 (synopsis "Continuous dice")
 (description "An implementation of continuous Dice")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 3.0.0))
  (menhir (>= 20210310))
  (alcotest :with-test)
  (cmdliner (>= 1.0))))