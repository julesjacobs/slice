# Dockerfile for Slice
# Builds both Slice and Dice from source with all dependencies
# Also includes Racket and Roulette for potential backend support

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
    numpy==1.22.3 \
    scipy==1.8.0 \
    matplotlib==3.5.1 \
    pandas==1.4.2 \
    sppl

# Install hyperfine
RUN wget -qO- https://github.com/sharkdp/hyperfine/releases/download/v1.18.0/hyperfine-v1.18.0-$(uname -m)-unknown-linux-gnu.tar.gz | tar xz && \
    mv hyperfine-*/hyperfine /usr/local/bin/ && \
    rm -rf hyperfine-*

# Install Racket (using apt for compatibility)
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:plt/racket && \
    apt-get update && \
    apt-get install -y racket && \
    rm -rf /var/lib/apt/lists/*

# Install Z3 solver (required by Rosette/Roulette)
RUN apt-get update && \
    apt-get install -y z3 && \
    rm -rf /var/lib/apt/lists/*

# We'll use the local rsdd with added Roulette compatibility functions

# Set up Z3 for Rosette (handle ARM64 architecture)
RUN if [ "$(uname -m)" = "aarch64" ]; then \
        # For ARM64, use the system Z3 we already installed
        RACKET_VERSION=$(racket --version | grep -o '[0-9]\+\.[0-9]\+' | head -1) && \
        mkdir -p /home/opam/.local/share/racket/${RACKET_VERSION}/pkgs/rosette/bin && \
        ln -sf /usr/bin/z3 /home/opam/.local/share/racket/${RACKET_VERSION}/pkgs/rosette/bin/z3 && \
        chown -R opam:opam /home/opam/.local; \
    fi

# Clone and install Roulette from source (skip documentation build)
RUN git clone https://github.com/camoy/roulette.git /opt/roulette && \
    cd /opt/roulette && \
    raco pkg install --auto --batch --no-docs ./roulette/ ./roulette-lib

# Switch back to opam for building
USER opam

# Copy Dice directory first (contains our patched rsdd)
COPY --chown=opam:opam dice /home/opam/artifact/dice

# Clean up any existing build artifacts
USER root
RUN rm -rf /home/opam/artifact/dice/rsdd/target /home/opam/artifact/dice/_build

# Prepare cargo permissions for the build
USER root
RUN mkdir -p /home/opam/.cargo && \
    chown -R opam:opam /home/opam/.cargo && \
    # Ensure opam owns the entire dice directory recursively
    chown -R opam:opam /home/opam/artifact/dice && \
    chmod -R u+w /home/opam/artifact/dice && \
    # Remove lock file to avoid conflicts
    rm -f /home/opam/artifact/dice/rsdd/Cargo.lock

# Switch to opam user and build Dice (which will also build rsdd)
USER opam
RUN cd /home/opam/artifact/dice && \
    rm -rf _build && \
    eval $(opam env) && \
    dune build

# Copy the built rsdd library to system locations
USER root
RUN RACKET_VERSION=$(racket --version | grep -o '[0-9]\+\.[0-9]\+' | head -1) && \
    mkdir -p /home/opam/.local/share/racket/${RACKET_VERSION}/lib/ && \
    cp /home/opam/artifact/dice/_build/default/lib/dllrsdd.so /home/opam/.local/share/racket/${RACKET_VERSION}/lib/librsdd.so && \
    cp /home/opam/artifact/dice/_build/default/lib/dllrsdd.so /usr/local/lib/librsdd.so && \
    ldconfig && \
    chown -R opam:opam /home/opam/.local || true

USER opam

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
ENV PATH="/home/opam/artifact/slice/_build/default/bin:/home/opam/artifact/dice/_build/default/bin:/usr/local/bin:${PATH}"
ENV PYTHONUNBUFFERED=1

# Make slice.py executable
RUN chmod +x /home/opam/artifact/slice.py

# Create a dice wrapper script for the test command
RUN echo '#!/bin/bash\n/home/opam/artifact/dice/_build/default/bin/dice.exe "$@"' > /home/opam/artifact/dice/dice && \
    chmod +x /home/opam/artifact/dice/dice

# Switch back to opam user for runtime
USER opam
WORKDIR /home/opam/artifact

# Default command - show help using slice.py
CMD ["python3", "slice.py", "--help"]