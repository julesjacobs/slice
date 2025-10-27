#!/bin/bash

# Exit if any command fails
set -e

TIME=false

# Parse flags
if [[ "$1" == "--time" ]]; then
    TIME=true
    shift
fi

# Check for input argument
if [ "$#" -ne 1 ]; then
    echo "Usage: ./run_roulette.sh [--time] <file.rkt>"
    exit 1
fi

# Get the input file
input_file="$1"

# Check if the file exists
if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found."
    exit 1
fi

# If --time is specified, modify output.rkt
if [ "$TIME" = true ]; then
    # Check if start timing line already exists
    if ! grep -q "(define start (current-inexact-milliseconds))" $input_file; then
        sed -i '1a (define start (current-inexact-milliseconds))' $input_file
    fi

    # Check if end timing block already exists
    if ! grep -q "(define end (current-inexact-milliseconds))" $input_file; then
        cat <<'EOF' >> $input_file
(define end (current-inexact-milliseconds))
(define elapsed (/ (- end start) 1000.0))
(eprintf "~a\n" (~r elapsed #:precision 3))
EOF
    fi

    # Run modified file
    racket "$input_file"
else
    # Normal execution
    racket "$input_file"
fi
