#!/bin/bash
# Running slice compiler with a bash script

PRINT_ALL=false
BACKEND="dice"  # Default backend
TIME=false

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
    --time)
      TIME=true
      shift
      ;;
    *)
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
  echo "Usage: $0 [--print-all] [--dice|--roulette] [--time] <slice-file>"
  echo "Options:"
  echo "  --print-all   Print detailed output"
  echo "  --dice        Use Dice backend (default)"
  echo "  --roulette    Use Roulette backend"
  echo "  --time        Print elapsed time to stderr"
  exit 1
fi

INPUT_ABS=$(realpath "$INPUT")
OUTPUT_EXT=$([ "$BACKEND" = "roulette" ] && echo "rkt" || echo "dice")

# ================[ MAIN EXECUTION ]================

if [ "$BACKEND" = "roulette" ]; then 
  # ROULETTE
  if [ "$TIME" = true ]; then
    echo "#lang roulette/example/disrupt" > "../output.$OUTPUT_EXT"
    # always write non-print-all version to output file
    ./_build/default/bin/main.exe --backend "$BACKEND" --time "$INPUT_ABS" 2>/dev/null >> "../output.$OUTPUT_EXT"

    if [ "$PRINT_ALL" = true ]; then
      ./_build/default/bin/main.exe --print-all --backend "$BACKEND" --time "$INPUT_ABS" 2>/dev/null
    else
      ./_build/default/bin/main.exe --backend "$BACKEND" --time "$INPUT_ABS" 2>/dev/null
    fi

    # print time to stderr
    ./_build/default/bin/main.exe --backend "$BACKEND" --time "$INPUT_ABS" 2>&1 >/dev/null | grep '^[0-9]*\.[0-9]*$' >&2

  else
    echo "#lang roulette/example/disrupt" > "../output.$OUTPUT_EXT"
    ./_build/default/bin/main.exe --backend "$BACKEND" "$INPUT_ABS" >> "../output.$OUTPUT_EXT"
    if [ "$PRINT_ALL" = true ]; then
      ./_build/default/bin/main.exe --print-all --backend "$BACKEND" "$INPUT_ABS"
    else
      ./_build/default/bin/main.exe --backend "$BACKEND" "$INPUT_ABS"
    fi
  fi

else 
  # DICE
  if [ "$TIME" = true ]; then
    # always write non-print-all version to output file
    ./_build/default/bin/main.exe --backend "$BACKEND" --time "$INPUT_ABS" 2>/dev/null > "../output.$OUTPUT_EXT"

    if [ "$PRINT_ALL" = true ]; then
      ./_build/default/bin/main.exe --print-all --backend "$BACKEND" --time "$INPUT_ABS" 2>/dev/null
    else
      ./_build/default/bin/main.exe --backend "$BACKEND" --time "$INPUT_ABS" 2>/dev/null
    fi

    # print time to stderr
    ./_build/default/bin/main.exe --backend "$BACKEND" --time "$INPUT_ABS" 2>&1 >/dev/null | grep '^[0-9]*\.[0-9]*$' >&2

  else
    ./_build/default/bin/main.exe --backend "$BACKEND" "$INPUT_ABS" > "../output.$OUTPUT_EXT"
    if [ "$PRINT_ALL" = true ]; then
      ./_build/default/bin/main.exe --print-all --backend "$BACKEND" "$INPUT_ABS"
    else
      ./_build/default/bin/main.exe --backend "$BACKEND" "$INPUT_ABS"
    fi
  fi
fi