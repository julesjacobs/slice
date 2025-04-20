(lang dune 3.11)
(using menhir 2.1)

(name contdice)

(generate_opam_files true)

(source
 (github julesjacobs/contdice))

(authors "Jules Jacobs")
(license MIT)
(documentation https://github.com/julesjacobs/contdice)

(package
 (name contdice)
 (synopsis "Continuous dice library")
 (description "Library for probabilistic programming with continuous distributions")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 3.0))
  (menhir (>= 20201216))
  (stats (>= 0.1))
  (cmdliner (>= 1.1.0))
  (ounit2 (>= 2.2.6))
  )
 (tags ("probabilistic-programming" "continuous-distributions" dice))
 (bug_reports https://github.com/julesjacobs/contdice/issues)
 (homepage https://github.com/julesjacobs/contdice)
)