#!/bin/bash
# Workflow for contdice

# ================[ CDICE ]================
# Check that an argument was passed
if [ $# -lt 1 ]; then
  echo "Usage: $0 <example_program>"
  exit 1
fi

EXAMPLE=$1

# Go into cdice directory and run the command
cd cdice || exit 1
OUTPUT=$(dune exec -- bin/main.exe "$EXAMPLE")

# ================[ DICE ]================
cd ..
cd dice || exit 1

dune exec dice "$OUTPUT"

