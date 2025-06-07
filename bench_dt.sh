#!/bin/bash

# Initialize hyperfine command with common options
RESULTS_FILE="benchmarks/decision-trees/results.md"
HYPERFINE_CMD="hyperfine -w3 --export-markdown $RESULTS_FILE"

# Process each directory in benchmarks/decision-trees/
for dir in benchmarks/decision-trees/DT4 benchmarks/decision-trees/DT14 benchmarks/decision-trees/DT16 benchmarks/decision-trees/DT44; do
  # Process Python files
  for py_file in "$dir"/*.py; do
    if [ -f "$py_file" ]; then
      name=$(basename "$py_file" .py)
      HYPERFINE_CMD+=" --command-name \"$name (sppl)\" \"python3 $py_file\""
    fi
  done
  
  # Process CDice files
  for cdice_file in "$dir"/*.cdice; do
    if [ -f "$cdice_file" ]; then
      name=$(basename "$cdice_file" .cdice)
      HYPERFINE_CMD+=" --command-name \"$name (contdice)\" \"./run_contdice.sh $cdice_file\""
    fi
  done

  # Run for pieces 16, 32, 64, 128, 256, 4096
  # Process CDice files
  for cdice_file in "$dir"/*.cdice; do
    if [ -f "$cdice_file" ]; then
      name=$(basename "$cdice_file" .cdice)
      HYPERFINE_CMD+=" --command-name \"$name (contdice)\" \"./run_contdice.sh $cdice_file\""
    fi
  done
  
done

# Execute the full hyperfine command
echo "Running benchmarks..."
eval "$HYPERFINE_CMD"

echo "Benchmark results saved to $RESULTS_FILE"