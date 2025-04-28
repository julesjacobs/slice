# Test script for benchmarks

#!/usr/bin/env python3
import subprocess
import time
import sys
import os
from pathlib import Path

'''
Usage:
$ python3 test.py: runs all benchmarks using ./run_contdice.sh
$ python3 test.py --run-as-separate: runs all benchmarks separating cdice (cdice/run_cdice.sh) from dice (dice/run_dice.sh)
$ python3 test.py --print-outputs: runs all benchmarks printing outputs for contdice and sppl
'''
def run(command, shell=False):
    result = subprocess.run(command, shell=shell, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return result

def run_and_time(description, command, shell=False):
    print(f"Running {description}...")
    start = time.time()
    result = subprocess.run(command, shell=shell, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    duration = time.time() - start
    print(f"time {duration}")
    output = result.stdout.decode('utf-8')
    return output
    
def main():
    print("Running the tests in benchmarks...\n")
    run_as_separate = "--run-as-separate" in sys.argv
    print_outputs = "--print-outputs" in sys.argv

    benchmark_dir = Path("benchmarks").resolve()
    if not benchmark_dir.exists():
        print("Error: benchmarks directory not found.")
        return
    
    for path in sorted(benchmark_dir.glob("**/*")):
        if run_as_separate:
            # Time contdice as separate cdice and dice components
            if path.suffix == ".py":
                run(["python3", str(path)])  # avoid cold starts / compilation time
                result_sppl = run_and_time(f"sppl: {path}", ["python3", str(path)])
                if print_outputs: print(result_sppl)
                print("\n")
            elif path.suffix == ".cdice":
                original_dir = os.getcwd()
                try:
                    # Change to cdice directory and run cdice component
                    os.chdir("./cdice")
                    run(["./run_cdice.sh", str(path)])
                    cdice_prog = run_and_time(f"contdice -- cdice: {path}", ["./run_cdice.sh", str(path)])
                    # Change back to original directory
                    os.chdir(original_dir)
                    # Change to dice directory and run dice component
                    os.chdir("./dice")
                    run(["./run_dice.sh", "../output.dice"])
                    result_contdice = run_and_time(f"contdice -- dice: {path}", ["./run_dice.sh", "../output.dice"])
                    if print_outputs: print(result_contdice)
                finally:
                    # Ensure we return to the original directory even if an error occurs
                    os.chdir(original_dir)
        else:
            # Time contdice as one component
            if path.suffix == ".py":
                run(["python3", str(path)])  # avoid cold starts
                result_sppl = run_and_time(f"sppl: {path}", ["python3", str(path)])
                if print_outputs: print(result_sppl)
                print("\n")
            elif path.suffix == ".cdice":
                run(["./run_contdice.sh", str(path)])  # avoid cold starts
                result_contdice = run_and_time(f"contdice: {path}", ["./run_contdice.sh", str(path)])
                if print_outputs: print(result_contdice)

if __name__ == "__main__":
    main()
