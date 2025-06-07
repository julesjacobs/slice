#!/bin/bash

# Create a hyperfine command with all benchmarks
CMD="hyperfine -w3 --shell=none --show-output --export-markdown results.md"

for dir in DT4 DT16; do
  for file in "$dir"/*.py "$dir"/*.slice; do
    name=$("$file")
      echo "$name"
      CMD+=" --command-name \"$name\" "
  done
done

# Run the full hyperfine command
eval "$CMD"