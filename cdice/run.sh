#!/bin/bash

# Determine the target path within the examples directory
if [ -z "$1" ]; then
  TARGET="examples" # No argument, target the whole directory
  echo "No specific example specified, running all examples in: $TARGET"
else
  TARGET="examples/$1" # Argument given, target specific file within examples
  echo "Running specific example: $TARGET"
fi

# Build the project
echo "Building project..."
dune build

# Check if build was successful
if [ $? -ne 0 ]; then
  echo "Build failed, exiting."
  exit 1
fi

# Run the executable on the target
echo "Executing..."
dune exec -- bin/main.exe "$TARGET"