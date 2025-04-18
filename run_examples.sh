#!/bin/bash

# Build the project
dune build

# Run examples
echo "Running all examples in the examples directory:"
dune exec -- bin/main.exe examples

# Optionally run a specific example
if [ "$1" != "" ]; then
  echo -e "\nRunning specific example: $1"
  dune exec -- bin/main.exe "$1"
fi