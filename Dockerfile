FROM ocaml/opam

WORKDIR /home/opam/slice

RUN opam switch create 4.14.1

RUN eval $(opam env)

RUN opam depext mlcuddidl

