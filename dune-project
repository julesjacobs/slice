(lang dune 3.0)
(name contdice)
(generate_opam_files true)

(package
 (name contdice)
 (synopsis "Continuous dice")
 (description "An implementation of continuous Dice")
 (depends
  (ocaml (>= 4.13.0))
  (dune (>= 3.0.0))
  (alcotest :with-test)))