#!/bin/bash

# ================[ WORKFLOW FOR SLICE ]================
# First run slice, which converts a continuous program to an equivalent discretized one
# Then run either dice or racket depending on the backend specified

# Default to dice if no backend specified
BACKEND="dice"
OUTPUT_EXT="dice"  # Default output extension

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dice)
      BACKEND="dice"
      OUTPUT_EXT="dice"
      shift
      ;;
    --roulette)
      BACKEND="roulette"
      OUTPUT_EXT="rkt"
      shift
      ;;
    *)
      # First non-flag argument is the example program
      if [[ -z "$EXAMPLE" ]]; then
        EXAMPLE=$1
        shift
      else
        echo "Error: Unexpected argument: $1"
        exit 1
      fi
      ;;
  esac
done

if [ -z "$EXAMPLE" ]; then
  echo "Usage: $0 [--dice|--roulette] <example-program>"
  echo "Options:"
  echo "  --dice       Use Dice backend (default)"
  echo "  --roulette   Use Roulette backend"
  exit 1
fi

EXAMPLE_ABS=$(realpath "$EXAMPLE")

# ================[ SLICE ]================
cd slice || exit 1

if [ "$BACKEND" = "roulette" ]; then
  # For roulette, prepend the #lang line
  {
    echo "#lang roulette/example/disrupt"
    ./_build/default/bin/main.exe --backend "$BACKEND" "$EXAMPLE_ABS"
  } > "../output.$OUTPUT_EXT"
else
  # For dice, just run normally
  ./_build/default/bin/main.exe --backend "$BACKEND" "$EXAMPLE_ABS" > "../output.$OUTPUT_EXT"
fi

cd ..

# ================[ BACKEND EXECUTION ]================
case $BACKEND in
  "dice")
    # Run dice backend
    cd dice || exit 1
    ./_build/default/bin/dice.exe "../output.$OUTPUT_EXT" -show-size -flip-lifting
    ;;
  "roulette")
    # Run racket for roulette backend
    racket "output.$OUTPUT_EXT"
    ;;
esac