#!/bin/bash
# Can alternatively use the slice.py CLI

# ================[ WORKFLOW FOR SLICE ]================
# First run slice, which converts a continuous program to an equivalent discretized one
# Then run either dice or racket depending on the backend specified

# Default to dice if no backend specified
BACKEND="dice"
OUTPUT_EXT="dice"  # Default output extension
TIME_FLAG=false

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
    --time)
      TIME_FLAG=true
      shift
      ;;
    *)
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
  echo "Usage: $0 [--dice|--roulette] [--time] <example-program>"
  echo "Options:"
  echo "  --dice       Use Dice backend (default)"
  echo "  --roulette   Use Roulette backend"
  echo "  --time       Measure and print total execution time"
  exit 1
fi

EXAMPLE_ABS=$(realpath "$EXAMPLE")

# ================[ SLICE ]================
cd slice || exit 1

if [ "$BACKEND" = "roulette" ]; then
  if [ "$TIME_FLAG" = true ]; then
    slice_time_roulette=$(./run_slice.sh --roulette --time "$EXAMPLE_ABS" 2>&1 >/dev/null)
  else
    ./run_slice.sh --roulette "$EXAMPLE_ABS" > /dev/null 2>&1
  fi

else
  if [ "$TIME_FLAG" = true ]; then
    slice_time_dice=$(./run_slice.sh --time "$EXAMPLE_ABS" 2>&1 >/dev/null)
  else
    ./run_slice.sh "$EXAMPLE_ABS" > /dev/null 2>&1
  fi
fi

cd ..

# ================[ BACKEND EXECUTION ]================
case $BACKEND in
  "dice")
    # Run dice backend
    cd dice || exit 1
    if [ "$TIME_FLAG" = true ]; then
      ./run_dice.sh --time "../output.$OUTPUT_EXT" 2>/dev/null
      backend_time_dice=$(./run_dice.sh --time "../output.$OUTPUT_EXT" 2>&1 >/dev/null)
      total_time=$(echo "$slice_time_dice + $backend_time_dice" | bc -l)
      echo "$total_time" >&2
    else
      ./run_dice.sh "../output.$OUTPUT_EXT"
    fi
    ;;
  "roulette")
    # Run racket for roulette backend
    cd roulette || exit 1
    if [ "$TIME_FLAG" = true ]; then
      ./run_roulette.sh --time "../output.$OUTPUT_EXT" 2>/dev/null
      backend_time_roulette=$(./run_roulette.sh --time "../output.$OUTPUT_EXT" 2>&1 >/dev/null)
      total_time=$(echo "$slice_time_roulette + $backend_time_roulette" | bc -l)
      echo "$total_time" >&2
    else
      ./run_roulette.sh "../output.$OUTPUT_EXT"
    fi
    ;;
esac