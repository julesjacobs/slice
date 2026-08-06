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

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --storm)
      RUN_STORM=true
      shift
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
  echo "Usage: $0 [--storm] [--dice|--roulette] [--time] <example-program>"
  echo "Options:"
  echo "  --storm      Run Storm on the generated DTMC"
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

  if [ "$TIME_FLAG" = true ]; then
    ./_build/default/bin/main.exe --storm --time "$EXAMPLE_ABS"
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

  run_storm_prop() {
    local prop="$1"
    local out
    out="$(storm --explicit "$TRA" "$LAB" --prop "$prop")"
    printf '%s\n' "$out" >&2
    printf '%s\n' "$out" | awk '/Result/ {print $NF}' | tail -n 1
  }

  # "accept" labels true terminal states; "done" is reached from every
  # successfully returned value.  Normalizing by P(F done), rather than by
  # the probability of avoiding observation failure, excludes divergence.
  true_mass="$(run_storm_prop 'P=? [ F "accept" ]')"
  return_mass="$(run_storm_prop 'P=? [ F "done" ]')"
  if [[ -z "$true_mass" || -z "$return_mass" ]]; then
    echo "Error: could not parse Storm output for returned-value normalization." >&2
    exit 1
  fi
  echo "--------------------------------------------------------------"
  echo ""

  awk -v true_mass="$true_mass" -v return_mass="$return_mass" '
    BEGIN {
      printf "Raw true-return mass P(F \"accept\") = %.17g\n", true_mass;
      printf "Successful-return mass P(F \"done\") = %.17g\n", return_mass;
      if (return_mass == 0) {
        print "Returned-value posterior is undefined (P(F \"done\") = 0).";
      } else {
        printf "Returned-value posterior P(true | successful return) = %.17g\n", true_mass / return_mass;
      }
    }'

  if grep -q " error" "$LAB"; then
    error_mass="$(run_storm_prop 'P=? [ F "error" ]')"
    if [[ -z "$error_mass" ]]; then
      echo "Error: could not parse Storm output for runtime-error mass." >&2
      exit 1
    fi
    awk -v error_mass="$error_mass" '
      BEGIN { printf "Runtime-error mass P(F \"error\") = %.17g\n", error_mass; }'
  fi

  if grep -q "has_observe" "$LAB"; then
    no_fail_mass="$(run_storm_prop 'P=? [ G !"fail" ]')"
    if [[ -z "$no_fail_mass" ]]; then
      echo "Error: could not parse Storm output for observation-success conditioning." >&2
      exit 1
    fi
    awk -v true_mass="$true_mass" -v no_fail_mass="$no_fail_mass" '
      BEGIN {
        printf "Observation-success mass P(G !\"fail\") = %.17g\n", no_fail_mass;
        if (no_fail_mass == 0) {
          print "Observation-success-conditioned probability is undefined (P(G !\"fail\") = 0).";
        } else {
          printf "P(F \"accept\" | G !\"fail\") = %.17g\n", true_mass / no_fail_mass;
        }
      }'
  fi
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
