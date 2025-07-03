# Slice: Type-Directed Discretization of Continuous Probabilistic Programs

Slice is a probabilistic programming language that automatically discretizes continuous distributions to enable exact inference. This repository contains the implementation described in our POPL 2025 paper.

## Quick Start

### Using Docker (Recommended)

```bash
# Build the Docker image
docker build -t slice .

# Run an example (Dice backend only - Roulette requires Racket which is not included)
docker run --rm slice python3 slice.py --dice run examples/paper/2.1_simple_uniform.slice

# Run benchmarks (Dice backend only)
docker run --rm slice python3 slice.py --dice benchmark

# Get help
docker run --rm slice python3 slice.py --help
```

#### Running Your Own .slice Programs with Docker

To run your own `.slice` files, mount your current directory and use the `/data` prefix:

```bash
# Run a .slice file from your current directory
docker run --rm -v "$(pwd):/data" slice python3 slice.py --dice run /data/yourprogram.slice
```

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
