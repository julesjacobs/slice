# Slice: Type-Directed Discretization of Continuous Probabilistic Programs

Slice is a probabilistic programming language that automatically discretizes continuous distributions to enable exact inference. This repository contains the implementation described in our OOPSLA 2026 paper.

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
@article{slice2026,
author = {Wu, Katherine and Jacobs, Jules and Batz, Kevin and Silva, Alexandra},
title = {Type-Directed Discretization of Probabilistic Programs},
year = {2026},
issue_date = {October 2026},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
volume = {10},
number = {OOPSLA2},
url = {https://doi.org/10.1145/3839534},
doi = {10.1145/3839534}, 
}
```
