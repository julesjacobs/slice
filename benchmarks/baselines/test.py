# Test script for benchmarks

#!/usr/bin/env python3
import subprocess
import time
import sys
import os
import string
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np
from benchmarks.scaling.gen import * # custom library for generating programs

from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

'''
Usage:
1. run benchmarks in simple/
2. run benchmarks in baselines/
3. run benchmarks in bayesian-networks/
4. test for good asymptotic scaling

$ python3 test.py [simple | baselines | bayesian-networks]: runs all [simple | baselines | bayesian-networks] benchmarks using ./run_slice.sh
$ python3 test.py [simple | baselines | bayesian-networks] --run-separate: runs all [simple | baselines | bayesian-networks] benchmarks separating slice (slice/run_slice.sh) from dice (dice/run_dice.sh)
$ python3 test.py [simple | baselines | bayesian-networks] --print-outputs: runs all [simple | baselines | bayesian-networks] benchmarks printing outputs for slice and sppl
$ python3 test.py --test-scaling: tests asymptotic scaling for a variety of programs
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


def run_as_one(benchmark_dir):
    # List to store benchmark results: (benchmark_name, slice_time, sppl_time)
    benchmark_results = []
    for path in sorted(benchmark_dir.glob("**/*")):
        if not path.is_file():
            continue 
        benchmark_name = path.stem
        # --- SPPL ---
        if path.suffix == ".py":
            run(["python3", str(path)])  # warm-up run
            total_sppl_time = 0
            for _ in range(10):
                result_sppl, sppl_time = run_and_time(f"sppl: {path}", ["python3", str(path)])
                total_sppl_time += sppl_time
            sppl_time = total_sppl_time / 10
            # update benchmark_results list with record
            for i, (n, t1, _) in enumerate(benchmark_results):
                if n == benchmark_name:
                    benchmark_results[i] = (benchmark_name, t1, sppl_time)
                    break
            else:
                benchmark_results.append((benchmark_name, None, sppl_time))
            if print_outputs_flag: 
                print(result_sppl)
            print("\n")
            
        # --- SLICE ---
        elif path.suffix == ".slice":
            run(["./run_slice.sh", str(path)])  # warm-up run
            total_slice_time = 0
            for _ in range(10):
                result_slice, slice_time = run_and_time(f"slice: {path}", ["./run_slice.sh", str(path)])
                total_slice_time += slice_time
            slice_time = total_slice_time / 10
            # update benchmark_results list with new record
            for i, (n, _, t2) in enumerate(benchmark_results):
                if n == benchmark_name:
                    benchmark_results[i] = (benchmark_name, slice_time, t2)
                    break
            else:
                benchmark_results.append((benchmark_name, slice_time, None))
            if print_outputs_flag: 
                print(result_slice)
                
    return benchmark_results


def run_as_separate(benchmark_dir):
    # List to store benchmark results: (benchmark_name, (slice_time, slice_time), sppl_time)
    benchmark_results = []
    for path in sorted(benchmark_dir.glob("**/*")):
        if not path.is_file():
            continue
        benchmark_name = path.stem
        # --- SPPL ---
        if path.suffix == ".py":
            run(["python3", str(path)])  # warm-up run
            total_sppl_time = 0
            for _ in range(10):
                result_sppl, sppl_time = run_and_time(f"sppl: {path}", ["python3", str(path)])
                total_sppl_time += sppl_time
            sppl_time = total_sppl_time / 10
            # update benchmark_results list with record
            for i, (n, (t1, t2), _) in enumerate(benchmark_results):
                if n == benchmark_name:
                    benchmark_results[i] = (benchmark_name, (t1, t2), sppl_time)
                    break
            else:
                benchmark_results.append((benchmark_name, (None, None), sppl_time))
            if print_outputs_flag: 
                print(result_sppl)
            print("\n")
            
        # --- SLICE ---
        elif path.suffix == ".slice":
            original_dir = os.getcwd()
            try:
                # Run slice component
                os.chdir("./slice")
                run(["./run_slice.sh", str(path)])  # warm-up run
                total_slice_time = 0
                for _ in range(10):
                    slice_output, slice_time = run_and_time(f"slice -- slice: {path}", ["./run_slice.sh", str(path)])
                    total_slice_time += slice_time
                slice_time = total_slice_time / 10
                # Run dice component
                os.chdir(original_dir)
                os.chdir("./dice")
                run(["./run_dice.sh", "../output.dice"])  # warm-up run
                total_dice_time = 0
                for _ in range(10):
                    dice_output, dice_time = run_and_time(f"slice -- dice: {path}", ["./run_dice.sh", "../output.dice"])
                    total_dice_time += dice_time
                dice_time = total_dice_time / 10
                # update benchmark_results list with new record
                for i, (n, (_, _), t3) in enumerate(benchmark_results):
                    if n == benchmark_name:
                        benchmark_results[i] = (benchmark_name, (slice_time, dice_time), t3)
                        break
                else:
                    benchmark_results.append((benchmark_name, (slice_time, dice_time), None))
                if print_outputs_flag: 
                    print(dice_output)
            finally:
                os.chdir(original_dir)
    return benchmark_results
        
        
def test_asymptotic_scaling_as_one():
    # List to store benchmark results: (program_size, slice_duration, sppl_duration)  
    benchmark_results =[]
    original_dir = os.getcwd()
    
    # Generate programs of increasing size, linearly
    for i in range(1, 21):
        prog_size = i*5
        
        # --- SLICE ---
        program_slice, last_var = build_alternating_guard_slice_3(i*5, i)
        path = Path("parametrize.slice")
        path.write_text(program_slice)
        command = ["./run_slice.sh", str(path)]
        # do a warm-up run first
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        total_slice_time = 0
        for _ in range(10):
            start = time.time()
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            duration = time.time() - start
            total_slice_time += duration
        slice_duration = total_slice_time / 10
        output = result.stdout.decode('utf-8')
        if print_outputs_flag: print(output)
        print(f"slice time {slice_duration}")
        
        # --- SPPL ---
        total_sppl_time = 0
        os.chdir("./slice")
        for _ in range(10):
            # Convert the slice program to an sppl program using --to-sppl
            command = ["dune", "exec", "--", "bin/main.exe", "--to-sppl", "../parametrize.slice"]
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            output = result.stdout.decode('utf-8')
            start = time.time()
            compiler = SPPL_Compiler(f'''{output}''')
            namespace = compiler.execute_module()
            model = Id('model')
            event = (model >= 1.0)
            output = namespace.model.prob(event)
            duration = time.time() - start
            total_sppl_time += duration
            
        sppl_duration = total_sppl_time / 10
        if print_outputs_flag: print(output)
        print(f"sppl time {sppl_duration}")
        os.chdir(original_dir)
        
        benchmark_results.append((prog_size, slice_duration, sppl_duration)) 
    return benchmark_results


def test_asymptotic_scaling_as_separate():
    # List to store benchmark results: (program_size, (slice_duration, dice_duration), sppl_duration)  
    benchmark_results =[]
    original_dir = os.getcwd()
    
    # Generate programs of increasing size, linearly
    for i in range(1, 11):
        prog_size = i*5
        
        # --- SLICE ---
        program_slice, last_var = build_alternating_guard_slice_3(i*5, i)
        path = Path("parametrize.slice").resolve()
        path.write_text(program_slice)
        # Run slice component
        os.chdir("./slice")
        command = ["./run_slice.sh", str(path)]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        total_slice_time = 0
        for _ in range(10):
            start = time.time()
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            duration = time.time() - start
            total_slice_time += duration
        slice_duration = total_slice_time / 10
        output = result.stdout.decode('utf-8')
        if print_outputs_flag: print(output)
        print(f"slice -- slice time {slice_duration}")
        # Run dice component
        os.chdir(original_dir)
        os.chdir("./dice")
        command = ["./run_dice.sh", "../output.dice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        total_dice_time = 0
        for _ in range(10):
            start = time.time()
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            duration = time.time() - start
            total_dice_time += duration
        dice_duration = total_dice_time / 10
        output = result.stdout.decode('utf-8')
        if print_outputs_flag: print(output)
        print(f"slice -- dice time {dice_duration}")
        os.chdir(original_dir)
        
        # --- SPPL ---
        total_sppl_time = 0
        os.chdir("./slice")
        for _ in range(10):
            # Convert the slice program to an sppl program using --to-sppl
            command = ["dune", "exec", "--", "bin/main.exe", "--to-sppl", "../parametrize.slice"]
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            output = result.stdout.decode('utf-8')
            start = time.time()
            compiler = SPPL_Compiler(f'''{output}''')
            namespace = compiler.execute_module()
            model = Id('model')
            event = (model >= 1.0)
            output = namespace.model.prob(event)
            duration = time.time() - start
            total_sppl_time += duration
            
        sppl_duration = total_sppl_time / 10
        if print_outputs_flag: print(output)
        print(f"sppl time {sppl_duration}")
        os.chdir(original_dir)
        
        benchmark_results.append((prog_size, (slice_duration, dice_duration), sppl_duration)) 
    return benchmark_results


def generate_bar_graphs(benchmark_results, output_path):
    plt.figure(figsize=(12, 8))
    width = 0.35
    
    # Create x-axis positions for each benchmark
    x = range(len(benchmark_results))
    
    if run_separate_flag:
        # For separate mode: red bar for sppl, stacked blue bars for slice+dice
        names = [b[0] if b[0] is not None else "Unnamed" for b in benchmark_results]
        slice_times = [b[1][0] if b[1] and b[1][0] is not None else 0 for b in benchmark_results]
        dice_times = [b[1][1] if b[1] and b[1][1] is not None else 0 for b in benchmark_results]
        sppl_times = [b[2] if b[2] is not None else 0 for b in benchmark_results]
        
        # Plot sppl bars (red) on the left side of each x position
        bars_sppl = plt.bar([i - width/2 for i in x], sppl_times, width, color='red', label='SPPL')
        
        # Plot stacked slice + dice bars (dark and light blue) on the right side
        bars_slice = plt.bar([i + width/2 for i in x], slice_times, width, color='darkblue', label='Slice')
        bars_dice = plt.bar([i + width/2 for i in x], dice_times, width, bottom=slice_times, 
                           color='lightblue', label='Dice')
        
        plt.title('Benchmark Results: Separate Slice and Dice Components')
    else:
        # For combined mode: red bar for sppl, blue bar for slice
        names = [b[0] if b[0] is not None else "Unnamed" for b in benchmark_results]
        slice_times = [b[1] if b[1] is not None else 0 for b in benchmark_results]
        sppl_times = [b[2] if b[2] is not None else 0 for b in benchmark_results]
        
        # Plot sppl bars (red) on the left side of each x position
        bars_sppl = plt.bar([i - width/2 for i in x], sppl_times, width, color='red', label='SPPL')
        
        # Plot slice bars (blue) on the right side
        bars_slice = plt.bar([i + width/2 for i in x], slice_times, width, color='blue', label='Slice')
        
        plt.title('Benchmark Results: Combined Slice Component')
    
    plt.xticks(x, names, rotation=45, ha='right')
    plt.ylabel('Time (seconds)')
    plt.legend()
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    print(f"Saved plot to {output_path}")
    
    

def generate_line_graphs(benchmark_results, output_path):
    '''
    Args:
    benchmark_results: list of tuples (prog_size, slice_times, sppl_times) or (prog_size, (slice_times, dice_times), sppl_times) if --run-separate flag is passed
    output_path: .png file
    '''
    if run_separate_flag:
        prog_size = [b[0] if b[0] is not None else 0 for b in benchmark_results]
        slice_times = [b[1][0] if b[1] and b[1][0] is not None else 0 for b in benchmark_results]
        dice_times = [b[1][1] if b[1] and b[1][1] is not None else 0 for b in benchmark_results]
        sppl_times = [b[2] if b[2] is not None else 0 for b in benchmark_results]
        plt.plot(prog_size, sppl_times, label="SPPL", color="red", marker="o")
        plt.plot(prog_size, slice_times, label="Slice", color="darkblue", marker="o")
        plt.plot(prog_size, dice_times, label="Dice", color="lightblue", marker="o")
        
    else:
        prog_size = [b[0] if b[0] is not None else 0 for b in benchmark_results]
        slice_times = [b[1] if b[1] is not None else 0 for b in benchmark_results]
        sppl_times = [b[2] if b[2] is not None else 0 for b in benchmark_results]
        plt.plot(prog_size, sppl_times, label="SPPL", color="red", marker="o")
        plt.plot(prog_size, slice_times, label="Slice", color="blue", marker="o")

    plt.xlabel("Program Size")
    plt.ylabel("Execution Time (seconds)")
    plt.title("Sppl vs Slice Execution Time")
    plt.legend()
    plt.grid(True)
    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.show()
        

run_separate_flag = False
print_outputs_flag = False
test_scaling_flag = False

def main():
    global run_separate_flag, print_outputs_flag, test_scaling_flag
    run_separate_flag = "--run-separate" in sys.argv
    print_outputs_flag = "--print-outputs" in sys.argv
    test_scaling_flag = "--test-scaling" in sys.argv
    
    # --- Test asymptotic scaling ---
    print(f"Testing asymptotic scaling...\n")
    if test_scaling_flag:
        if run_separate_flag:
            # Test scaling as separate slice and dice components
            benchmark_results = test_asymptotic_scaling_as_separate()
            # Generate the appropriate line graph
            if benchmark_results:
                output_file = "images/scaling.png"
                generate_line_graphs(benchmark_results, output_file)
        else:
            # Test scaling as one slice component
            benchmark_results = test_asymptotic_scaling_as_one()
            # Generate the appropriate line graph
            if benchmark_results:
                output_file = "images/scaling.png"
                generate_line_graphs(benchmark_results, output_file)
        return
        

    # --- Run tests in benchmarks directory ---
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
    if run_separate_flag:
        # Time slice as separate slice and dice components
        benchmark_results = run_as_separate(benchmark_dir)
        # Generate the appropriate bar graph
        if benchmark_results:
            output_file = "images/bars.png"
            generate_bar_graphs(benchmark_results, output_file)
    else: 
        # Time slice as one slice component
        benchmark_results = run_as_one(benchmark_dir)
        # Generate the appropriate bar graph
        if benchmark_results:
            output_file = "images/bars.png"
            generate_bar_graphs(benchmark_results, output_file)
        

if __name__ == "__main__":
    main()