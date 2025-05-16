#!/bin/bash

# Build the project
echo "Building project..."
dune build

# Check if build was successful
if [ $? -ne 0 ]; then
  echo "Build failed, exiting."
  exit 1
fi

COMMAND_OR_FILE=$1
FILE_PATH=""

if [ "$COMMAND_OR_FILE" = "discretize" ]; then
  if [ $# -lt 2 ]; then
    echo "Usage: $0 discretize <filepath>"
    exit 1
  fi
  FILE_PATH=$2
  echo "Discretizing (plain): $FILE_PATH"
  dune exec -- bin/main.exe "$FILE_PATH"
elif [ "$COMMAND_OR_FILE" = "interp" ]; then
  if [ $# -lt 2 ]; then
    echo "Usage: $0 interp <filepath>"
    exit 1
  fi
  FILE_PATH=$2
  echo "Interpreting (verbose): $FILE_PATH"
  dune exec -- bin/main.exe --print-all "$FILE_PATH"
else
  # Default: assume first arg is filename (or no args for help from main.exe)
  # Pass all original args, include --print-all
  echo "Executing bin/main.exe with arguments: --print-all $@"
  dune exec -- bin/main.exe --print-all "$@"
fi