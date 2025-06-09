#!/bin/bash
# run_docker.sh - Simple wrapper for running Slice via Docker with proper volume mounting

# Ensure we're in the right directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Create directories if they don't exist
mkdir -p artifact_results
mkdir -p images/scaling

# Function to show usage
usage() {
    echo "Usage: $0 <command> [options]"
    echo ""
    echo "Commands:"
    echo "  run <file.slice>        Run a Slice program"
    echo "  test                    Test installation"
    echo "  benchmark [type]        Run benchmarks (all, dt, scaling)"
    echo "  examples --list         List available examples"
    echo "  examples --run <name>   Run a specific example"
    echo "  build                   Build Slice components inside Docker"
    echo "  docker-build            Build the Docker image from Dockerfile"
    echo ""
    echo "Examples:"
    echo "  $0 docker-build                         # Build Docker image"
    echo "  $0 run examples/tutorial/coin.slice"
    echo "  $0 benchmark dt"
    echo "  $0 benchmark scaling --timeout 600"
    echo "  $0 test"
    exit 1
}

# Check if no arguments provided
if [ $# -eq 0 ]; then
    usage
fi

# Parse command
COMMAND=$1
shift

# Base docker command with volume mounts
DOCKER_CMD="docker run --rm \
    -v $(pwd)/artifact_results:/home/opam/artifact/artifact_results \
    -v $(pwd)/images:/home/opam/artifact/images \
    slice python3 slice.py"

# Execute based on command
case "$COMMAND" in
    run)
        if [ $# -eq 0 ]; then
            echo "Error: Please specify a .slice file to run"
            usage
        fi
        # Mount the input file's directory
        FILE_PATH="$1"
        FILE_DIR=$(dirname "$FILE_PATH")
        FILE_NAME=$(basename "$FILE_PATH")
        
        docker run --rm \
            -v $(pwd)/artifact_results:/home/opam/artifact/artifact_results \
            -v $(pwd)/"$FILE_DIR":/home/opam/artifact/"$FILE_DIR" \
            slice python3 slice.py run "$FILE_DIR/$FILE_NAME" "${@:2}"
        ;;
        
    test)
        $DOCKER_CMD test "$@"
        ;;
        
    benchmark)
        $DOCKER_CMD benchmark "$@"
        ;;
        
    examples)
        $DOCKER_CMD examples "$@"
        ;;
        
    build)
        echo "Building components inside Docker..."
        $DOCKER_CMD build "$@"
        ;;
        
    docker-build)
        echo "Building Docker image from Dockerfile..."
        docker build -t slice .
        if [ $? -eq 0 ]; then
            echo "Docker image 'slice' built successfully!"
        else
            echo "Error: Failed to build Docker image"
            exit 1
        fi
        ;;
        
    *)
        echo "Error: Unknown command '$COMMAND'"
        usage
        ;;
esac