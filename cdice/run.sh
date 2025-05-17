#!/bin/bash
dune build || exit 1
dune exec -- bin/main.exe --print-all "$@"