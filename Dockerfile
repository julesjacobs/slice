FROM ocaml/opam

WORKDIR contdice

RUN opam switch create 4.14.1

RUN eval $(opam env)

RUN opam depext mlcuddidl