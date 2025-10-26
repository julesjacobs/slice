#!/bin/bash

# Exit if any command fails
set -e

# Initialize flag
TIME_FLAG=false

# Parse arguments
if [ "$#" -lt 1 ]; then
    echo "Usage: ./run_roulette.sh [--time] <file.rkt>" >&2
    exit 1
fi

# Check for --time flag
if [ "$1" == "--time" ]; then
    TIME_FLAG=true
    shift
fi

# Get the input file
input_file="$1"

# Check if the file exists
if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found." >&2
    exit 1
fi

# Run the Racket program, optionally with timing
if [ "$TIME_FLAG" = true ]; then
    start=$(date +%s.%N)
    racket "$input_file"
    end=$(date +%s.%N)
    duration=$(awk -v start="$start" -v end="$end" 'BEGIN {printf "%.6f", end - start}')
    echo "$duration" >&2
else
    racket "$input_file"
fi

