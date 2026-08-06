# Slice: Type-Directed Discretization of Continuous Probabilistic Programs

Slice is a probabilistic programming language that automatically discretizes continuous distributions for discrete inference. This repository contains the implementation described in our POPL 2025 paper.

The formal transformation uses exact real interval masses and is semantics-preserving. The current implementation instead computes those masses with binary64 OCaml GSL CDF routines. Discrete backends may perform exact inference relative to the resulting floating-point weights, but this does not give certified equality with the ideal continuous source semantics. The artifact currently provides neither symbolic masses nor certified numerical error bounds.

## Quick Start

### Using Docker (Recommended)

Use the provided wrapper script `slice-docker`:

```bash
# First, make the script executable (one time only)
chmod +x slice-docker

# Run any .slice file in your current directory
./slice-docker yourprogram.slice

# Run with verbose output
./slice-docker yourprogram.slice --print-all

# Convert to SPPL format
./slice-docker yourprogram.slice --to-sppl

# Run examples from the repository
./slice-docker examples/paper/2.1_simple_uniform.slice

# Get help
./slice-docker
```

The wrapper script automatically handles Docker setup and file mounting for you.

### Local Installation

Requires OCaml 4.14+ and Python 3.8+.

```bash
# Install OCaml dependencies
opam install dune menhir cmdliner ounit2 gsl core core_unix \
             ppx_sexp_conv sexplib ppx_deriving yojson ctypes \
             ctypes-foreign bignum mlcuddidl

# Build Slice and Dice
cd slice && dune build && cd ..
cd dice && dune build && cd ..

# Install Python dependencies
pip install matplotlib sppl

# Run an example
python3 slice.py run examples/paper/2.1_simple_uniform.slice

# Optional: Install Racket for Roulette backend support
# See: https://racket-lang.org/
# After installing Racket, install Roulette: raco pkg install roulette
```

## Usage

The `slice.py` script provides a unified interface for all operations:

```bash
# Run a Slice program
python3 slice.py run <file.slice>

# Convert to SPPL
python3 slice.py run <file.slice> --to-sppl

# Run benchmarks
python3 slice.py benchmark              # All benchmarks
python3 slice.py benchmark dt           # Decision trees only
python3 slice.py benchmark scaling      # Scaling benchmarks only

# Test installation
python3 slice.py test

# List examples
python3 slice.py examples --list

# Build components
python3 slice.py build
```

## Project Structure

- `slice/` - The Slice compiler (continuous → discrete)
- `dice/` - The Dice inference engine (exact inference on discrete programs)
- `examples/` - Example Slice programs organized by category:
  - `tutorial/` - Simple tutorial examples
  - `paper/` - Examples from the POPL 2025 paper
  - `features/` - Examples demonstrating specific language features
  - `tests/` - Test cases and edge cases
- `benchmarks/` - Performance benchmarks:
  - `decision-trees/` - Decision tree benchmarks (DT4, DT14, DT16, DT44)
  - `scaling/` - Scaling benchmarks
  - `baselines/` - Baseline implementations
- `tex/` - Paper source

## Paper

The paper "Slice: Type-Directed Discretization of Continuous Probabilistic Programs" is available in the `tex/` directory.

## Citation

```bibtex
@inproceedings{slice2025,
  title={Slice: Type-Directed Discretization of Continuous Probabilistic Programs},
  author={[Authors]},
  booktitle={Proceedings of the ACM on Programming Languages (POPL)},
  year={2025}
}
```
