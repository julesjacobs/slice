# Dockerfile for Slice
# Builds both Slice and Dice from source with all dependencies

FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

# Switch to root for system package installation
USER root

# Install system dependencies
RUN apt-get update && apt-get install -y \
    # Missing build tools
    wget \
    pkg-config \
    m4 \
    # Required libraries
    libgmp-dev \
    libffi-dev \
    libgsl-dev \
    # Python 3
    python3 \
    python3-pip \
    # Utilities
    time \
    # Rust for rsdd
    cargo \
    && rm -rf /var/lib/apt/lists/*

# Switch to opam user for OCaml operations
USER opam
WORKDIR /home/opam/artifact

# Install OCaml dependencies (using the pre-installed switch)
RUN eval $(opam env) && \
    opam depext -y mlcuddidl && \
    opam install -y \
    dune \
    menhir \
    cmdliner \
    ounit2 \
    gsl \
    core \
    core_unix \
    ppx_sexp_conv \
    sexplib \
    ppx_deriving \
    yojson \
    ctypes \
    ctypes-foreign \
    bignum \
    mlcuddidl

# Install Python packages (as root)
USER root
RUN pip3 install --upgrade pip setuptools wheel && \
    pip3 install --no-cache-dir \
    matplotlib \
    sppl

# Install hyperfine
RUN wget -qO- https://github.com/sharkdp/hyperfine/releases/download/v1.18.0/hyperfine-v1.18.0-$(uname -m)-unknown-linux-gnu.tar.gz | tar xz && \
    mv hyperfine-*/hyperfine /usr/local/bin/ && \
    rm -rf hyperfine-*

# Switch back to opam for building
USER opam

# Copy and build Dice first
COPY --chown=opam:opam dice /home/opam/artifact/dice
RUN cd /home/opam/artifact/dice && \
    rm -rf _build && \
    eval $(opam env) && \
    dune build

# Copy and build Slice
COPY --chown=opam:opam slice /home/opam/artifact/slice
RUN cd /home/opam/artifact/slice && \
    rm -rf _build && \
    eval $(opam env) && \
    dune build

# Copy the rest of the artifact files
USER root
COPY --chown=opam:opam . /home/opam/artifact/

# Set up environment variables
ENV PATH="/home/opam/artifact/slice/_build/default/bin:/home/opam/artifact/dice/_build/default/bin:${PATH}"
ENV PYTHONUNBUFFERED=1

# Make slice.py executable
RUN chmod +x /home/opam/artifact/slice.py

# Switch back to opam user for runtime
USER opam
WORKDIR /home/opam/artifact

# Default command - show help using slice.py
CMD ["python3", "slice.py", "--help"]