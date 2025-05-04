# Test script for benchmarks

#!/usr/bin/env python3
import subprocess
import time
import sys
import os
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np

'''
Usage:
1. run benchmarks in simple/
2. run benchmarks in baselines/
3. run benchmarks in bayesian-networks/
4. test for good asymptotic scaling

$ python3 test.py [simple | baselines | bayesian-networks]: runs all [simple | baselines | bayesian-networks] benchmarks using ./run_contdice.sh
$ python3 test.py [simple | baselines | bayesian-networks] --run-separate: runs all [simple | baselines | bayesian-networks] benchmarks separating cdice (cdice/run_cdice.sh) from dice (dice/run_dice.sh)
$ python3 test.py [simple | baselines | bayesian-networks] --print-outputs: runs all [simple | baselines | bayesian-networks] benchmarks printing outputs for contdice and sppl
$ python3 test.py --test-scale: tests asymptotic scaling for a variety of programs
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
    return output, duration

def generate_bar_graphs(benchmark_results, output_path, separate_mode=False):
    plt.figure(figsize=(12, 8))
    
    # Filter out benchmarks with no timing data
    benchmarks = [b for b in benchmark_results if any(t is not None for t in b[1:])]
    if not benchmarks:
        print("No benchmark data to plot")
        return
    
    names = [b[0] for b in benchmarks]
    x = np.arange(len(names))
    width = 0.35
    
    if separate_mode:
        # For separate mode: red bar for sppl, stacked blue bars for cdice+dice
        sppl_times = [b[1] if b[1] is not None else 0 for b in benchmarks]
        cdice_times = [b[2] if b[2] is not None else 0 for b in benchmarks]
        contdice_times = [b[3] if b[3] is not None else 0 for b in benchmarks]
        
        # Plot sppl bars (red)
        bars_sppl = plt.bar(x - width/2, sppl_times, width, color='red', label='SPPL')
        
        # Plot stacked cdice + dice bars (dark and light blue)
        bars_cdice = plt.bar(x + width/2, cdice_times, width, color='darkblue', label='CDice')
        bars_dice = plt.bar(x + width/2, contdice_times, width, bottom=cdice_times, 
                            color='lightblue', label='Dice')
        
        plt.title('Benchmark Results: Separate CDice and Dice Components')
    else:
        # For combined mode: red bar for sppl, blue bar for contdice
        sppl_times = [b[1] if b[1] is not None else 0 for b in benchmarks]
        contdice_times = [b[3] if b[3] is not None else 0 for b in benchmarks]
        
        # Plot sppl bars (red)
        bars_sppl = plt.bar(x - width/2, sppl_times, width, color='red', label='SPPL')
        
        # Plot contdice bars (blue)
        bars_contdice = plt.bar(x + width/2, contdice_times, width, color='blue', label='ContDice')
        
        plt.title('Benchmark Results: Combined ContDice Component')
    
    plt.xticks(x, names, rotation=45, ha='right')
    plt.ylabel('Time (seconds)')
    plt.legend()
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    print(f"Saved plot to {output_path}")


def run_as_one(benchmark_dir):
    # List to store benchmark results: (name, sppl_time, cdice_time, contdice_time)
    benchmark_results = []
    
    for path in sorted(benchmark_dir.glob("**/*")):
        if not path.is_file():
            continue
        
        benchmark_name = path.name
        sppl_time = None
        cdice_time = None
        contdice_time = None
        
        # --- SPPL ---
        if path.suffix == ".py":
            run(["python3", str(path)])  # warm-up run
            total_sppl_time = 0
            for _ in range(10):
                result_sppl, sppl_time = run_and_time(f"sppl: {path}", ["python3", str(path)])
                total_sppl_time += sppl_time
            sppl_time = total_sppl_time / 10
            if print_outputs: 
                print(result_sppl)
            print("\n")
            
        # --- CONTDICE ---
        elif path.suffix == ".cdice":
            run(["./run_contdice.sh", str(path)])  # warm-up run
            total_contdice_time = 0
            for _ in range(10):
                result_contdice, contdice_time = run_and_time(f"contdice: {path}", ["./run_contdice.sh", str(path)])
                total_contdice_time += contdice_time
            contdice_time = total_contdice_time / 10
            if print_outputs: 
                print(result_contdice)
                
        benchmark_results.append((benchmark_name, sppl_time, cdice_time, contdice_time))
    return benchmark_results


def run_as_separate(benchmark_dir):
    # List to store benchmark results: (name, sppl_time, cdice_time, contdice_time)
    benchmark_results = []
    
    for path in sorted(benchmark_dir.glob("**/*")):
        if not path.is_file():
            continue
        
        benchmark_name = path.name
        sppl_time = None
        cdice_time = None
        dice_time = None
        
        # --- SPPL ---
        if path.suffix == ".py":
            run(["python3", str(path)])  # warm-up run
            total_sppl_time = 0
            for _ in range(10):
                result_sppl, sppl_time = run_and_time(f"sppl: {path}", ["python3", str(path)])
                total_sppl_time += sppl_time
            sppl_time = total_sppl_time / 10
            if print_outputs: 
                print(result_sppl)
            print("\n")
            
        # --- CONTDICE ---
        elif path.suffix == ".cdice":
            original_dir = os.getcwd()
            try:
                # Run cdice component
                os.chdir("./cdice")
                run(["./run_cdice.sh", str(path)])  # warm-up run
                total_cdice_time = 0
                for _ in range(10):
                    cdice_output, cdice_time = run_and_time(f"contdice -- cdice: {path}", ["./run_cdice.sh", str(path)])
                    total_cdice_time += cdice_time
                cdice_time = total_cdice_time / 10
                
                # Run dice component
                os.chdir(original_dir)
                os.chdir("./dice")
                run(["./run_dice.sh", "../output.dice"])  # warm-up run
                total_dice_time = 0
                for _ in range(10):
                    dice_output, dice_time = run_and_time(f"contdice -- dice: {path}", ["./run_dice.sh", "../output.dice"])
                    total_dice_time += dice_time
                dice_time = total_dice_time / 10
                
                if print_outputs: 
                    print(dice_output)
            finally:
                os.chdir(original_dir)
        benchmark_results.append((benchmark_name, sppl_time, cdice_time, dice_time))
    return benchmark_results
        

run_separate = False
print_outputs = False

def main():
    global run_as_separate, print_outputs
    run_separate = "--run-separate" in sys.argv
    print_outputs = "--print-outputs" in sys.argv

    # Determine the benchmark directory based on arguments
    if "simple" in sys.argv:
        benchmark_dir = Path("benchmarks/simple").resolve()
    elif "bayesian-networks" in sys.argv:
        benchmark_dir = Path("benchmarks/bayesian-networks").resolve()
    elif "baselines" in sys.argv:  # Added support for baselines
        benchmark_dir = Path("benchmarks/baselines").resolve()
    else:
        benchmark_dir = Path("benchmarks").resolve()

    if not benchmark_dir.exists():
        print(f"Error: {benchmark_dir} directory not found.")
        return
    
    print(f"Running the tests in {benchmark_dir}...\n")
    if run_separate:
        # Time contdice as separate cdice and dice components
        benchmark_results = run_as_separate(benchmark_dir=benchmark_dir)
    else: 
        # Time contdice as one component
        benchmark_results = run_as_one(benchmark_dir=benchmark_dir)
        
    # Generate the appropriate bar graph
    if benchmark_results:
        output_file = "bars.png"
        generate_bar_graphs(benchmark_results, output_file, run_separate)

if __name__ == "__main__":
    main()