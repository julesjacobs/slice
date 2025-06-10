#!/bin/bash
# Running slice compiler with a bash script

PRINT_ALL=false
BACKEND="dice"  # Default backend

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --print-all)
      PRINT_ALL=true
      shift
      ;;
    --dice)
      BACKEND="dice"
      shift
      ;;
    --roulette)
      BACKEND="roulette"
      shift
      ;;
    *)
      # First non-flag argument is the input file
      if [[ -z "$INPUT" ]]; then
        INPUT=$1
        shift
      else
        echo "Error: Unexpected argument: $1"
        exit 1
      fi
      ;;
  esac
done

if [ -z "$INPUT" ]; then
  echo "Usage: $0 [--print-all] [--dice|--roulette] <slice-file>"
  echo "Options:"
  echo "  --print-all   Print detailed output"
  echo "  --dice        Use Dice backend (default)"
  echo "  --roulette    Use Roulette backend"
  exit 1
fi

INPUT_ABS=$(realpath "$INPUT")

# Determine output file extension based on backend
OUTPUT_EXT=$([ "$BACKEND" = "roulette" ] && echo "rkt" || echo "dice")

if [ "$BACKEND" = "roulette" ]; then
  # For roulette, prepend the #lang line
  {
    echo "#lang roulette/example/disrupt"
    if [ "$PRINT_ALL" = true ]; then
      ./_build/default/bin/main.exe --print-all --backend "$BACKEND" "$INPUT_ABS"
    else
      ./_build/default/bin/main.exe --backend "$BACKEND" "$INPUT_ABS"
    fi
  } | tee "../output.$OUTPUT_EXT"
else
  # For dice, run normally
  if [ "$PRINT_ALL" = true ]; then
    ./_build/default/bin/main.exe --print-all --backend "$BACKEND" "$INPUT_ABS" | tee "../output.$OUTPUT_EXT"
  else
    ./_build/default/bin/main.exe --backend "$BACKEND" "$INPUT_ABS" | tee "../output.$OUTPUT_EXT"
  fi
fi