#!/bin/bash

# Exit if any command fails
set -e

# Check if an argument is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: ./run_roulette.sh <file.rkt>"
    exit 1
fi

# Get the input file
input_file="$1"

# Check if the file exists
if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found."
    exit 1
fi

# Run the Racket program
racket "$input_file"
