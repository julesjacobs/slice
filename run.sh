#!/bin/bash
# Can alternatively use the slice.py CLI

# ================[ WORKFLOW FOR SLICE ]================
# First run slice, which converts a continuous program to an equivalent discretized one
# Then run either dice or racket depending on the backend specified

# Default to dice if no backend specified
BACKEND="dice"
OUTPUT_EXT="dice"  # Default output extension
TIME_FLAG=false
RUN_STORM=false
LIMIT=""

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --storm)
      RUN_STORM=true
      shift
      ;;
    --limit)
      if [ "$RUN_STORM" = true ]; then
        if [ $# -lt 2 ]; then
          echo "Error: --limit requires a value"
          exit 1
        fi
        LIMIT="$2"
        shift 2
      else
        echo "Error: --limit requires --storm"
        exit 1
      fi
      ;;
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
  echo "Usage: $0 [--storm] [--limit N] [--dice|--roulette] [--time] <example-program>"
  echo "Options:"
  echo "  --storm      Run Storm on the generated DTMC"
  echo "  --limit N    State limit for Storm DTMC construction (requires --storm)"
  echo "  --dice       Use Dice backend (default)"
  echo "  --roulette   Use Roulette backend"
  echo "  --time       Measure and print total execution time"
  exit 1
fi

EXAMPLE_ABS=$(realpath "$EXAMPLE")

# ================[ SLICE ]================
cd slice || exit 1

# Storm backend specified
if [ "$RUN_STORM" = true ]; then
  TRA="${EXAMPLE_ABS}.tra"
  LAB="${EXAMPLE_ABS}.lab"
  STATE_REW="${EXAMPLE_ABS}.state.rew"

  if [ -n "$LIMIT" ]; then
    ./_build/default/bin/main.exe --storm --limit "$LIMIT" "$EXAMPLE_ABS"
  else
    ./_build/default/bin/main.exe --storm "$EXAMPLE_ABS"
  fi

  if [ ! -f "$TRA" ]; then
    echo "Error: missing transition file: $TRA" >&2
    exit 1
  fi
  if [ ! -f "$LAB" ]; then
    echo "Error: missing label file: $LAB" >&2
    exit 1
  fi
  if [ ! -f "$STATE_REW" ]; then
    echo "Error: missing state rewards file: $STATE_REW" >&2
    exit 1
  fi

  storm --explicit "$TRA" "$LAB" \
    --staterew "$STATE_REW" \
    --prop 'R=? [ F "done" || F "accept" ]'
  cd ..
  exit 0
fi

# Roulette backend specified
if [ "$BACKEND" = "roulette" ]; then
  if [ "$TIME_FLAG" = true ]; then
    slice_time_roulette=$(./run_slice.sh --roulette --time "$EXAMPLE_ABS" 2>&1 >/dev/null)
  else
    ./run_slice.sh --roulette "$EXAMPLE_ABS" > /dev/null 2>&1
  fi

# Dice backend specified
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
