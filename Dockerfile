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
    # For installing Rust
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Rust using rustup for a newer version (system-wide)
ENV RUSTUP_HOME=/opt/rust
ENV CARGO_HOME=/opt/rust
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --no-modify-path && \
    chmod -R 777 /opt/rust
ENV PATH="/opt/rust/bin:${PATH}"

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

# Switch back to opam for building
USER opam

# Copy and build Dice components
COPY --chown=opam:opam dice /home/opam/artifact/dice

# Build dice with a modified approach to avoid cargo/dune conflicts
USER opam
RUN cd /home/opam/artifact/dice && \
    eval $(opam env) && \
    # Build rsdd separately with complete isolation
    cd rsdd && \
    export CARGO_HOME="/tmp/cargo-build" && \
    export CARGO_TARGET_DIR="/tmp/rsdd-target" && \
    mkdir -p $CARGO_HOME $CARGO_TARGET_DIR && \
    cargo build --release --target-dir $CARGO_TARGET_DIR && \
    cd .. && \
    # Create a modified dune file that doesn't rebuild rsdd
    cp lib/dune lib/dune.original && \
    echo '(menhir' > lib/dune.new && \
    echo ' (modules Parser)' >> lib/dune.new && \
    echo ' (flags "--table"))' >> lib/dune.new && \
    echo '' >> lib/dune.new && \
    echo '(ocamllex lexer)' >> lib/dune.new && \
    echo '' >> lib/dune.new && \
    echo '(library' >> lib/dune.new && \
    echo ' (libraries core core_unix.sys_unix cudd sexplib menhirLib ounit2 ctypes ctypes.foreign bignum)' >> lib/dune.new && \
    echo ' (foreign_archives rsdd)' >> lib/dune.new && \
    echo ' (library_flags (-linkall))' >> lib/dune.new && \
    echo ' (name diceLib)' >> lib/dune.new && \
    echo ' (preprocess (pps ppx_jane)))' >> lib/dune.new && \
    # Place the built libraries where dune expects them
    mkdir -p lib && \
    cp /tmp/rsdd-target/release/librsdd.so lib/dllrsdd.so && \
    cp /tmp/rsdd-target/release/librsdd.a lib/librsdd.a && \
    # Use the modified dune file
    mv lib/dune.new lib/dune && \
    # Now build dice
    echo "Building dice with modified dune configuration..." && \
    dune build bin/dice.exe && \
    # Verify the build
    if [ -f "_build/default/bin/dice.exe" ]; then \
        echo "SUCCESS: dice.exe built!"; \
        echo "Testing dice on example..."; \
        _build/default/bin/dice.exe resources/example.dice && \
        echo "Dice test passed!"; \
    else \
        echo "ERROR: dice.exe still not built"; \
        exit 1; \
    fi

# Copy and build Slice
COPY --chown=opam:opam slice /home/opam/artifact/slice
RUN cd /home/opam/artifact/slice && \
    rm -rf _build && \
    eval $(opam env) && \
    dune build || true

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