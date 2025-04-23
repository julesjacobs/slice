#!/bin/bash

# Build the project
echo "Building project..."
dune build

# Check if build was successful
if [ $? -ne 0 ]; then
  echo "Build failed, exiting."
  exit 1
fi

# Run the executable, passing all script arguments to it
echo "Executing bin/main.exe with arguments: $@"
dune exec -- bin/main.exe --print-all "$@"